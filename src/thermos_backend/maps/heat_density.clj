;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.maps.heat-density
  (:import (java.awt.image WritableRaster BandedSampleModel DataBuffer BufferedImage
                           ConvolveOp Kernel ColorModel Raster)
           (java.awt Point)
           (java.util Hashtable)
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (javax.imageio ImageIO)
           (org.apache.commons.math3.linear RealMatrix
                                            Array2DRowRealMatrix
                                            ArrayRealVector
                                            DefaultRealMatrixPreservingVisitor)))

(defn make-1d-gaussian-kernel
  "Make an array with length 2*bandwidth, with values of a Gaussian centred around the middle element.
  We scale the width of the Gaussian horizontally, otherwise we would end up with big values at the edges,
  which would make thigns look weird."
  [bandwidth]
  (let [width (+ bandwidth bandwidth)
        output (double-array width)
        ]
    (doseq [x (range width)]
      ;; Note we multiply x by 3 here to scale the graph horizontally by a factor of 1/3
      (let [numerator (Math/pow (* (- x bandwidth) 3) 2)
            denominator (* 2 (Math/pow bandwidth 2))
            coeff (/ 1 (* 2 Math/PI (Math/pow bandwidth 2)))
            value (* coeff (Math/exp (- (/ numerator denominator))))]
        (aset output x (double value))
        ))
    output))


(defn convolve-1d
  "Perform a 1d convolution"
  [^ArrayRealVector vector
   ^ArrayRealVector kernel
   ^doubles out-array]
  (let [kernel-width (.getDimension ^ArrayRealVector kernel)
        kernel-half-width (/ kernel-width 2)]
    ;; For all the elements in the vector, excluding the edge bits
    (dotimes [i (- (+ (.getDimension ^ArrayRealVector vector) 1) kernel-width)]
             (aset out-array (+ i kernel-half-width)
                   ;; The convolution value
                   (.dotProduct
                    ^ArrayRealVector (.getSubVector ^ArrayRealVector vector i kernel-width)
                    ^ArrayRealVector kernel)))))


(defn blur-raster
  "Takes a 2D array of doubles and returns an instance of Array2DRowRealMatrix with Gaussian blur applied."
  ^Array2DRowRealMatrix
  [^Array2DRowRealMatrix raster-matrix bandwidth]
  (let [raster-height (.getRowDimension raster-matrix)
        raster-width  (.getColumnDimension raster-matrix)
        out-array     (make-array Double/TYPE raster-width raster-height)

        ;; The reason we are using matrices here rather than double[][]s is that
        ;; they allow us to easily get the rows and columns
        ; raster-matrix (Array2DRowRealMatrix. raster)
        out-matrix (Array2DRowRealMatrix. ^"[[D" out-array)
        kernel (make-1d-gaussian-kernel bandwidth)
        kernel-vector (ArrayRealVector. ^doubles kernel)]

    ;; Loop over the rows first
    (dotimes [i raster-height]
      (let [row (.getRowVector raster-matrix i)
            out-array (double-array raster-width)]
        ;; For each row, compute the convolution of the row with the kernel
        (convolve-1d row kernel-vector out-array)
        (.setRow out-matrix i out-array)))

    ;; Loop over the columns
    (dotimes [i raster-width]
      (let [col (.getColumnVector out-matrix i)
            out-array (double-array raster-height)]
        (convolve-1d col kernel-vector out-array)
        (.setColumn out-matrix i out-array)))

    out-matrix
    ))


(defn- create-blank-2d-array [size]
  (into-array
   (for [_ (range size)]
     (double-array size))))

(defn- deg [rad] (* rad (/ 180.0 Math/PI)))
(defn- rad [deg] (* deg (/ Math/PI 180.0)))

(defn- asinh [d]
  (if (or (zero? d) (Double/isInfinite d)) 0
    (Math/log (+ d (Math/sqrt (+ 1 (* d d)))))))

(defn- metres-per-pixel [z lat]
  (/ (* 40075017.0 ;; circumference of earth
        (Math/cos (rad lat)))
     (Math/pow 2.0 (+ z 8.0))))

(defn- webmercator-coordinate->lonlat [x y z]
  (let [n (Math/pow 2.0 z)
        lon-deg (- (* 360 (/ x n)) 180)
        lat-rad (Math/atan (Math/sinh (* Math/PI (- 1 (/ (* 2 y) n)))))
        lat-deg (deg lat-rad)]
    [lon-deg lat-deg]))

(defn- webmercator-inverse [x y z size lon lat]
  (let [z2 (Math/pow 2 z)

        px (* z2 (/ (+ 180 lon) 360))
        py (* z2 0.5 (- 1 (/ (asinh (Math/tan (rad lat))) Math/PI)))

        px (* size (- px x))
        py (* size (- py y))
        ]
    [(int px) (int py)]))


(defn density-raster
  "Create and return a convolved raster.
    Expects arguments :x :y :z :size :bandwidth and :get-values
    x,y,z are the webmercator coordinates of the tile
    size is the pixel size of the tile
    bandwidth is the radius of the kernel in pixels

    get-values is a function which has the signature
    (fn [[lon lat] [lon lat] bw]) -> [[lon lat value]]
    so it returns a list of points with their values that are contained in the given 'rectangle', with the given bandwidth around it as well
    "
  [& {:keys [x y z size bandwidth get-values] :as args}]
  (let [tl (webmercator-coordinate->lonlat x y z)
        br (webmercator-coordinate->lonlat (inc x) (inc y) z)

        bandwidth-in-metres (* bandwidth (metres-per-pixel z (second br)))

        full-size (+ size bandwidth bandwidth)

        raster-array (create-blank-2d-array full-size)
        raster-matrix (Array2DRowRealMatrix. raster-array)

        ;; Project & sum values into raster. Because we're doing the
        ;; convolution on the projection, the area under the kernel
        ;; will not be correct unless we also squash the kernel.
        ;; However it doesn't matter much.

        values (get-values tl br bandwidth-in-metres)
        ]

    ;; Put the values into the matrix
    (doseq [{lat :y lon :x demand :demand} values]
      (let [[px py] (webmercator-inverse x y z size lon lat)
            px (+ bandwidth px)
            py (+ bandwidth py)]
        (when (and (< -1 px full-size)
                   (< -1 py full-size))
          (.setEntry raster-matrix px py (* 30 demand)))))

    ;; Apply the blur to the matrix and return
    (blur-raster raster-matrix bandwidth)))


(defn density-image
  "The main function
    Expects arguments as for `density-raster` above.
    Returns a byte array representation of the PNG image.
    "
  [& {:keys [x y z size bandwidth get-values] :as args}]
  (let [;; Put the args into a format which we can pass to (apply density-raster)
        args (->> args
                  (vec)
                  (apply concat))
        ;; Make an array of normalized pixel values
        ^Array2DRowRealMatrix raster (apply density-raster args)

        output-stream (ByteArrayOutputStream.)
        target-image  (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        max-value (atom 0)
        ]
    (dotimes [i size]
      (dotimes [j size]
        (let [value (.getEntry raster (+ i ^int bandwidth) (+ j ^int bandwidth))]
          (swap! max-value max value)
          (.setRGB target-image i j (Float/floatToIntBits (float value))))))

    (ImageIO/write target-image "png" output-stream)
    (.close output-stream)
    [@max-value (.toByteArray output-stream)]))

(defn colour-float-matrix [byte-array max-value is-heat]
  (let [image-in (ImageIO/read (ByteArrayInputStream. byte-array))]
    (if is-heat
      (dotimes [i (.getWidth image-in)]
        (dotimes [j (.getHeight image-in)]
          (let [value (.getRGB image-in i j)
                value (Float/intBitsToFloat (int value))
                value (if (zero? value) 0 (/ value max-value))
                c (min 255 (int (* value 255)))
                ;; we could pack a float directly into this image and not scale it
                c (unchecked-int
                   (bit-or
                    (bit-shift-left (int (min 255 (* (Math/pow value 0.5) 255))) 24)
                    (bit-shift-left c 16) ;; R
                    (bit-shift-left (int (/ (- 255 c) 1.7)) 8) ;; G
                    (int (- 255 c)) ;; B
                    ))
                ]
            (.setRGB image-in i j c))))

      ;; cold map is the same, but R and B are swapped.
      (dotimes [i (.getWidth image-in)]
        (dotimes [j (.getHeight image-in)]
          (let [value (.getRGB image-in i j)
                value (Float/intBitsToFloat (int value))
                value (if (zero? value) 0 (/ value max-value))
                c (min 255 (int (* value 255)))
                ;; we could pack a float directly into this image and not scale it
                c (unchecked-int
                   (bit-or
                    (bit-shift-left (int (min 255 (* (Math/pow value 0.5) 255))) 24)
                    (bit-shift-left (- 255 c) 16) ;; R
                    (bit-shift-left (int (/ (- 255 c) 1.7)) 8) ;; G
                    (int c) ;; B
                    ))
                ]
            (.setRGB image-in i j c))))
      )
    (let [os (ByteArrayOutputStream.)]
      (ImageIO/write image-in "png" os)
      (.close os)
      (.toByteArray os))))

