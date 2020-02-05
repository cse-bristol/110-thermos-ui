#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

query_box () {
    XMIN="${1?:xmin}"
    YMIN="${2?:ymin}"
    XMAX="${3?:xmax}"
    YMAX="${4?:ymax}"

    read XMIN YMIN < <(echo "$XMIN, $YMIN" | gdaltransform -s_srs EPSG:4326 -t_srs EPSG:27700 -output_xy)

    read XMAX YMAX < <(echo "$XMAX, $YMAX" | gdaltransform -s_srs EPSG:4326 -t_srs EPSG:27700 -output_xy)

    rings="[$XMIN,$YMAX],[$XMAX,$YMAX],[$XMAX,$YMIN],[$XMIN,$YMIN],[$XMIN,$YMAX]"
    
    AOI='{"geometryType":"esriGeometryPolygon","features":[{"geometry":{"rings":[['${rings}']],"spatialReference":{"wkid":27700,"latestWkid":27700}}}],"sr":{"wkid":27700,"latestWkid":27700}}'

    job=$(curl -s -G 'https://environment.data.gov.uk/arcgis/rest/services/gp/DataDownload/GPServer/DataDownload/submitJob' \
               --data-urlencode 'f=json' \
               --data-urlencode 'OutputFormat=0' \
               --data-urlencode 'RequestMode=Survey' \
               --data-urlencode "AOI=${AOI}" |
              jq -r .jobId)

    echo "started $job $AOI" 1>&2
    
    local jobstatus='esriJobExecuting'
    while [[ "${jobstatus}" == 'esriJobExecuting' ||
                 "${jobstatus}" == 'esriJobSubmitted'    
           ]]; do
        jobstate=$(curl -s -G 'https://environment.data.gov.uk/arcgis/rest/services/gp/DataDownload/GPServer/DataDownload/jobs/'${job} --data-urlencode 'f=json')

        jobstatus=$(echo "${jobstate}" |
            perl -ne 'print $1 if /"jobStatus":"([^"]+)"/')
        
        echo $jobstatus 1>&2
        sleep 5
    done

    curl -s 'https://environment.data.gov.uk/arcgis/rest/directories/arcgisjobs/gp/datadownload_gpserver/'$job'/scratch/results.json'
}


if (( $# != 5 )); then
    echo "usage: $0 <place> <xmin> <ymin> <xmax> <ymax>"
    exit 1
fi

OUTPUT_DIR="/thermos-lidar/$1/"
shift
mkdir -p -- "${OUTPUT_DIR}"

pushd "${OUTPUT_DIR}"

query_box "$@" |tee query-result.json|
    jq -r '[.data[]| select(.productName == "LIDAR Composite DSM")| .years[]| select(.year == "Latest")| .resolutions[]|.tiles[]| . + (.tileName | capture("LIDAR-DSM-(?<res>[^-]+)-(?<tile>.+)"))]|.[]|select(.res!="50CM")|.url' |
    while read -r url; do
        curl -O -J "${url}"
    done

for i in *.zip; do
    unzip "$i"
    rm -- "$i"
done

# chuck away 2M except where needed
find . -iname \*.asc -print | perl -ne 'print "$1\n" if /..(.+)_DSM/' | sort | uniq |
while read TILE; do
    if [[ -e "${TILE}_DSM_1M.asc" ]]; then
        rm -f -- "${TILE}_DSM_2M.asc"
    fi
done

for i in *.asc; do
    gdal_translate -of GTiff "$i" "${i%.asc}.tiff" >/dev/null 2>&1
    rm -- "$i"

    gdal_edit.py -a_srs EPSG:27700 "${i%.asc}.tiff"
done

popd
