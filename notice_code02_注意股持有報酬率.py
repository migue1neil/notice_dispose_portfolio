# -*- coding: utf-8 -*-
"""
Created on Sun Feb 12 22:58:50 2023

@author: Neil
"""

import os
import pandas as pd
import numpy as np
#from datetime import datetime
import re

os.chdir("C:/Users/Neil/Documents/git-repos/")

stock_price = pd.read_csv("./notice_stock_portfolio/tidy_up_data/stock_price2011_2022.csv")





stock_price.to_csv("./notice_stock_portfolio/tidy_up_data/stock_price2011_2022.csv", index=False)