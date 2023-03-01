conversion_Estados_financieros
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 25 09:20:02 2022

@author: cristian.guatemal
"""
import asposecells
import jpype.imports 
import array as arr
if not jpype.isJVMStarted():
  jpype.startJVM()
from asposecells.api import PdfSaveOptions, FileFormatType, Workbook, SheetSet
from asposecells.api import PdfCompliance
import os

# Para el año 2010
# inp = 'Y:\\IESS_2020\\Data\\SSC\\Balances_Financieros_Fondos\\2010\\'
# workbook = Workbook( str( inp +"12-2010_Balance_General_Administradoras_y_Fondos_A.xls") )

#arreglo de hojas a imprimir
# Hoja fondos excel 1
# sheets = arr.array('i',[1]) # el i indica que el array es de tipo int, en los corchetes vienen las hojas que quiere imprimir
#la hoja1 tiene indice 0, la hoja2 índice 1 y así

# index  = SheetSet(sheets) #convierte el arreglo en un sheetset

# Crear y configurar opciones de PDF
# pdfOptions = PdfSaveOptions()
# pdfOptions.setOnePagePerSheet(True)
# pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
# pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
# pdfOptions.setSheetSet(index)

# Convierte Excel a PDF
# ruta =  "C:\\Users\\cristian.guatemal\\Documents\\IESS\\IESS_estudio_actuarial_2010\\Resultados\\"
# f = os.listdir( ruta )[len(os .listdir( ruta ))-1]
# workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2010_1.pdf", pdfOptions)

#Hoja fondos excel 2
# workbook = Workbook( str( inp +"12-2010_Estado_de_Resultados_Adminsitradoras_y_Fondos_A.xls") )
# sheets = arr.array('i',[1]) 
# index  = SheetSet(sheets) 
# pdfOptions = PdfSaveOptions()
# pdfOptions.setOnePagePerSheet(False)
# pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
# pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
# pdfOptions.setSheetSet(index)
# workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2010_2.pdf", pdfOptions)

#Para el año 2011
# inp = 'Y:\\IESS_2020\\Data\\SSC\\Balances_Financieros_Fondos\\2011\\'
# workbook = Workbook( str( inp +"12-2011_Balance_General_Administradoras_y_Fondos_A.xls") )
# sheets = arr.array('i',[1]) 
# index  = SheetSet(sheets) 
# pdfOptions = PdfSaveOptions()
# pdfOptions.setOnePagePerSheet(True)
# pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
# pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
# pdfOptions.setSheetSet(index)
# workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2011_1.pdf", pdfOptions)

# workbook = Workbook( str( inp +"12-2011_Estado_de_Resultados_Administradoras_y_Fondos_A.xls") )
# sheets = arr.array('i',[1]) 
# index  = SheetSet(sheets) 
# pdfOptions = PdfSaveOptions()
# pdfOptions.setOnePagePerSheet(False)
# pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
# pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
# pdfOptions.setSheetSet(index)
# workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2011_2.pdf", pdfOptions)

#Para el año 2020
#Parte 1
inp = 'Y:\\IESS_2020\\Data\\SSC\\Balances_Financieros_Fondos\\2020\\'
workbook = Workbook( str( inp +"12_2020_BALANCE_CONSOLIDADO_Y_CONDENSADO_FONDOS_ADMINISTRADOS.xlsx") )
sheets = arr.array('i',[1]) 
index  = SheetSet(sheets) 
pdfOptions = PdfSaveOptions()
pdfOptions.setOnePagePerSheet(False)
pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
pdfOptions.setSheetSet(index)

ruta =  "C:\\Users\\cristian.guatemal\\Documents\\IESS\\IESS_estudio_actuarial_2020\\Resultados\\"
f = os.listdir( ruta )[len(os .listdir( ruta ))-1]
workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2020_1.pdf", pdfOptions)

#Parte 2
sheets = arr.array('i',[2]) 
index  = SheetSet(sheets) 
pdfOptions = PdfSaveOptions()
pdfOptions.setOnePagePerSheet(False)
pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
pdfOptions.setSheetSet(index)
workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2020_2.pdf", pdfOptions)

#Parte 2
sheets = arr.array('i',[2]) 
index  = SheetSet(sheets) 
pdfOptions = PdfSaveOptions()
pdfOptions.setOnePagePerSheet(False)
pdfOptions.setOutputBlankPageWhenNothingToPrint(False)
pdfOptions.setCompliance(PdfCompliance.PDF_A_1_B)
pdfOptions.setSheetSet(index)
workbook.save( str(ruta) + str(f) +"\\" +"IESS_SSC_balances_financieros_2020_2.pdf", pdfOptions)


