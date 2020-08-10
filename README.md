# SensitiveDemSpecies
Extending the analysis on sensitive species developed by Rindorf et al., (2020, Journal of Applied Ecology) Are fish sensitive to trawling recovering in the Northeast Atlantic? 

Analysis based on a population model (Gislason et al., 2008 https://doi.org/10.1093/icesjms/fsn035), a collection of traits, exploitation of demersal fish species (Walker et al., 2017 https://doi.org/10.1093/icesjms/fsw250), and abundances from European trawl surveys (https://datras.ices.dk/)

R Code scripts
<load.clean.surveys.R> Downloading data from ICES Datras and cleaning (format and taxonomy)
<model.R> Population model to quantify species sensitivity to fishing
<abundance.sensitive.R> Developing species abundance indices across surveys for sensitive species
<presence.sensitive.R> Developing species presence indices across surveys for sensitive species
<autoloess.R> Additional function to automatically optimize loess fits

Results
<Sensitive.csv> results from running <model.R>
<Abundance.index.spp.pdf> results from running <abundance.sensitive.R>
<Presence.index.spp.pdf> results from running <presence.sensitive.R>
