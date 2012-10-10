#!/bin/bash

FILE=it_books_sale.rest

rst2html.py $FILE | blogspot-filter.py >$FILE.html
