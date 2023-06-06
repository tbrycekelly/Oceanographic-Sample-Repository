source('R/Database Construction.R')



sedtrap.db = init.db('database', 'sedtrap')

sedtrap.db$NGA = list()
sedtrap.db$CCE = list()
sedtrap.db$BLOOFINZ.GOM = list()
sedtrap.db$BLOOFINZ.IO = list()


file = '../NGA-Projects/Data/Sediment Trap/NGA Sediment Trap Inventory.xlsx'
inventory = read.xlsx(file, startRow = 2)


sedtrap.db$NGA = update.inventory(inventory, sedtrap.db$NGA)
backup(sedtrap.db)

summarize(sedtrap.db$NGA)

tmp = retreive(sedtrap.db$NGA, 'preparation$analysis.type', 'bSi')
summarize(tmp)
export(tmp)


tmp = retreive(sedtrap.db$NGA, 'collection$station', 'GAK5')
summarize(tmp)
export(tmp)


backup(sedtrap.db)