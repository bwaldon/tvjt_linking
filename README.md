# Modeling Behavior in Truth Value Judgment Task Experiments
## Brandon Waldon and Judith Degen 

### Requirements:

- WebPPL (https://github.com/probmods/webppl) (to perform Bayesian Data Analysis)
- R/RStudio + relevant packages (to analyze and visualize BDA results)

### Run BDA scripts: 

#### Binary condition

```
webppl inference_scripts/binary_script.wppl --random-seed 1571250871 --require webppl-json --require './node_modules/mathjs'
```
#### Ternary condition

```
webppl inference_scripts/tertiary_script.wppl --random-seed 1571250622 --require webppl-json --require './node_modules/mathjs'
```

#### Quaternary condition

```
webppl inference_scripts/quaternary_script.wppl --random-seed 1571251139 --require webppl-json --require './node_modules/mathjs'
```

#### Quinary condition

```
webppl inference_scripts/quinary_script.wppl --random-seed 1571251077 --require webppl-json --require './node_modules/mathjs'
```
