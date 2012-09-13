#-*- coding: utf-8 -*-
import xlrd, xlwt, sys

if len(sys.argv) < 3:
  print "usage: %s input_file.xls output_file.xls" % sys.argv[0]
  sys.exit(1)

input_file = sys.argv[1]
output_file = sys.argv[2]

book = xlrd.open_workbook(input_file)
sh = book.sheet_by_index(2)

# The 'fields' variable contains a list of fields from the original
# document in the order as they appear in the result document.
# Adding, removing, or reordering values of the 'fields' allow
# adjusting the format of the result document.
fields = [
  "Наименование товара",
  "Сертификат",
  "Артикул",
  "Код ГБ",
  "Цена с НДС",
  "Розничная цена",
  "Страна изготовитель",
  "Штрихкод",
  "Количество",
]

# The 'header' variable contains the first line of the original document.
# The line contains the field names.
header = sh.row(0)

# The 'mapping' variable will contain the list of indecies defining the
# columns of the orginal document corresponding to the items from 'fields'.
# For example, [1, 8, 0, 10, 16, 18, 2, 3, 7] means that "Наименование товара"
# will be taken from the column 1 ('B') of the original document,
# "Сертификат"- from the column 8 ('I'), etc.
mapping = []
# 'i' iterates over the fields of the result document.
for i in range(len(fields)):
  # 'j' iterates over the fields of the original document.
  for j in range(len(header)):
    if header[j].value.encode("utf-8") == fields[i]:
      mapping.append(j)

# Create an Excel book document.
wb = xlwt.Workbook()
# Create a page (sheet) named 'Sheet1' in that document.
ws = wb.add_sheet('Sheet1')

# 'i' iterates over the lines of the original document.
for i in range(sh.nrows):
  # 'j' iterates over the column indecies which we need to take
  # from the each line of the original document.
  for j in range(len(mapping)):
    value = sh.row(i)[mapping[j]].value
    if type(value) is unicode: value = value.strip("'")
    ws.write(i, j, value)
  
wb.save(output_file)
