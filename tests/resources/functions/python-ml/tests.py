import json
import numpy
import pandas
import sklearn
import tensorflow

def main(context):
    return context.res.json({
        'success': True,
        'numpy': numpy.__version__,
        'pandasdoc': pandas.__version__,
        'sklearn': sklearn.__version__,
        'tensorflow': tensorflow.__version__
    })