aux = r.getwd()
from pdf2docx import parse
from typing import Tuple
ruta = aux + '\\'+ 'Resultados'
f = os .listdir( ruta )[len(os .listdir( ruta ))-1]
pdf_file  = ruta + "\\"  + str(f) + "\\" + 'IESS_SSC_estudio_actuarial.pdf'
docx_file = ruta + "\\"  + str(f) + "\\" + 'IESS_SSC_estudio_actuarial.docx'
parse(pdf_file, docx_file, start=0, end=None)
