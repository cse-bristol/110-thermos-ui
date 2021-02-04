# Heat Degree Days

This file documents the process for creating `hdd.json`, which defines a heat degree day value for a set of polygons.

## Inputs

* file `nrg_chdd_a` from [eurostat's HDD data](https://ec.europa.eu/eurostat/web/energy/data/database) - yearly HDD data for the member states of the EU.
* small scale cultural geography map from [naturalearthdata](https://www.naturalearthdata.com/downloads/).

## Method

Eurostat HDD data is defined as follows [source](https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm#stat_pres1599744381054):

`If Tₘ ≤ 15°C Then [HDD = ∑ᵢ(18°C - Tⁱₘ)] Else [HDD = 0]` where Tⁱₘ is the mean air temperature of day i.

As HDD values are trending upwards over time, despite Eurostat data going back to 1979 only the average of the last 5 years was used.

However, the THERMOS heat demand model was trained using HDD data for Copenhagen where the base temperature was 17°C: `HDD = ∑ᵢ(17°C - Tⁱₘ)`.

To remove this discrepancy, the Eurostat HDD values are multiplied by `HDD(Copenhagen, 17°C)/HDD(Copenhagen, Eurostat)`.

#### `HDD(Copenhagen, 17°C)`:

```
2006  2990.5
2007  2819.8
2008  2901.1
2009  3134.4
2010  3642
2011  3007.6
2012  3248
2013  3190.9
2014  2745
2015  2885.8
2016  2910.7
2017  2986.8

avg   3038.55
```

For HDD(Copenhagen, 17°C), the average of all the years was used as the model was trained on all the years.

#### `HDD(Copenhagen, Eurostat)`

`2943.88466666667`

The average of the latest 5 years was used as in the other data from Eurostat.

#### Conclusion

All Eurostat HDD values were multiplied by `3038.55 / 2943.88` = `1.03215660396116`, and added as properties to the geoJSON from naturalearthdata.
