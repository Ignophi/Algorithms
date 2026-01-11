# Algorithms

This repository contains from-scratch implementations of decision-making and classification models, beginning with simple heuristics and progressing toward more complex algorithms, with an emphasis on understanding their underlying assumptions.

## Models

### The Minimalist (`The_minimalist.r`)
A fast-and-frugal **heuristic** that makes decisions using a single, randomly sampled cue, stopping at the first feature that discriminates between two options. The model assumes equal cue importance, ignores magnitude of differences, and relies on minimal information and computation.

### Take The Last (`Take_the_last.r`)
A fast-and-frugal **heuristic** that makes decisions by checking cues sequentially in an order determined by **recency of successful discrimination**. The model stops at the first cue that discriminates and predicts the option with the higher cue value. Cue importance is **adaptive but minimal**: after a successful decision, the discriminating cue is moved to the front of the search order. The model relies on ordinal comparisons only, can use recognition as a primary cue, and may treat missing values as informative depending on the NA heuristic.
