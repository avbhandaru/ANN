#!/usr/bin/env python3

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

if __name__ == "__main__":
    data = pd.read_csv(sys.argv[1])
    data.columns = ['epoch', 'loss']
    plt.plot(data['epoch'], data['loss'])
    plt.show()
