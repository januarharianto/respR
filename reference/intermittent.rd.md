# Respirometry data of the sea urchin, *Heliocidaris Erythrogramma*

Multiple measurements of oxygen consumption in a single sea urchin,
*Heliocidaris erythrogramma*, obtained using intermittent flow
respirometry. The experiment was conducted at the Sydney Institute of
Marine Science in Sydney, Australia. There are a total of 3 replicates
showing declining oxygen, separated by flushes where new water was added
showing increasing oxygen. Data was collected using a Vernier Optical DO
probe (ODO-BTA).

## Usage

``` r
intermittent.rd
```

## Format

A data frame object consisting of 2 columns (time and dissolved oxygen)
and 4831 rows (approx 80 min of data).

## Details

- Dissolved oxygen units: `mg/L`

- Time units: `seconds`

- Chamber volume (L): `2.379`

- Specimen ash-free dry mass (kg): `0.006955`

Replicate structure (Rows - Experiment section):

- `1:1900` - Replicate 1

- `1901:2100` - Flush 1

- `2101:3550` - Replicate 2

- `3551:3900` - Flush 2

- `3901:4831` - Replicate 3

## Author

Nicholas Carey
