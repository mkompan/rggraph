#!/bin/bash

(echo '#include "phi3.h"'; ./expand $*) | cpp | sed "/^#/ d" | python expand.py

