# ouvrir quelques librairies et charger les codes MysSQL
library(DBI)
library(RMySQL)

drv=dbDriver("MySQL")

# ouverture de la connection a la base de donnee
con = dbConnect(drv, user="UVC.reader", dbname="Underwater Visual Census",password="Mayfish976",host="162.38.198.139")

# liste les differentes tables disponibles
tabdbListTables(con)


# load the tables UVC
Webfish_annotation=dbReadTable(con, dbListTables(con)[17])
