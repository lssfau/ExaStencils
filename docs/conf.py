#!/usr/bin/env python3

import sys
import os
import sphinx_rtd_theme

sys.path.append(os.path.abspath('.'))

html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]
html_theme = 'sphinx_rtd_theme'

source_suffix = {
    '.rst': 'restructuredtext',
}

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.mathjax',
]

html_static_path = ['_static']

master_doc = 'index'

project = 'ExaStencils'
copyright = '2021, LSS Team'

version = ''
release = ''

pygments_style = 'sphinx'
