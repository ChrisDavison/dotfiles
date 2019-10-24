import os
import sys

import matplotlib
import matplotlib.pyplot as plt

import numpy as np
import scipy as sp
import statsmodels.api as sm
import pingouin as pg
import pandas as pd

from sqlalchemy import create_engine

from pathlib import Path

get_ipython().magic(u"%reload_ext autoreload")
get_ipython().magic(u"%autoreload 2")

pd.set_option("display.max_columns", 80)
pd.set_option("display.precision", 2)
