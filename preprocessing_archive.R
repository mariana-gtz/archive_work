library(data.table)

### Joining files
new_colnames <- c("id", "nombre", "ciudad",
                  "campus", "ciclo_numero", "programa", "promedio_up", 
                  "id_preparatoria", "preparatoria")

# Create empty data.table to store enrollment records all campus
archive <- as.data.table(matrix(ncol=length(new_colnames), nrow=0))

# Rename columns
setnames(archive, old=names(archive), new=new_colnames)

for (campus in c("UPANA", "UPAGS", "UPGDL")) {
  # Read file 
  path <- "../data/archive_2015_2020/archive_since_05112020.xlsx"
  #path <- "D:/Mariana/ACM-W/DataScience/archive_since_05112020.xlsx"
  archive_campus <- as.data.table(readxl::read_xlsx(path, sheet=campus))
  
  # Select columns
  rel_cols <- c("EMPLID", "NAME", "CIUDAD",  
                "INSTITUTION", "ADMIT_TERM", "ACAD_PROG", "UP_PROMEDIO_UP",
                "ID_PREPA", "PREPA")
  archive_campus <- archive_campus[, rel_cols, with=FALSE] 
  
  # Rename columns
  setnames(archive_campus, old=names(archive_campus), new=new_colnames)
  
  # Append to data.table with all ARCHIVE results of all CAMPUS
  archive <- rbind(archive, archive_campus)
}

# Extract SCHOOL TERM from string range
extract_ciclo <- function(number){
  year <- substr(number, 2, 3)
  month <- ifelse(substr(number, 4, 4) == "2", yes="Enero", no="Agosto")
  ciclo <- paste0(month, " 20", year)
}

archive$ciclo <- sapply(archive$ciclo_numero, extract_ciclo)

# Remove ciclo_numero column
archive[, ciclo_numero := NULL]

#Change promedio_up NULL values to NA
archive[archive$promedio_up=="NULL"] = NA

# Remove comma in name
archive$nombre <- gsub(",", " ", archive$nombre)

# Extract states list from ciudad column
states <- unique(unlist(archive$ciudad))

# Extract stet from ciudad column
#states_alias <- c("Puebla", "Querétaro", "Jalisco", "Tamaulipas", "Estado de México", "Tabasco",
                  #"Morelos", "Yucatán", "Veracruz de Ignacio de la Llave", "Oaxaca", "Tlaxcala", "Michoacán de Ocampo", 
                  #"Hidalgo", "Chihuahua", "Chiapas", "Extranjero", "Estado de México", "Sonora",
                  #"Guerrero", "Quintana Roo", "Ciudad de México", "Champeche", "Sinaloa", "Baja California",
                  #"Coahuila", "Aguascalientes", "Guanajuato", "San Luis Potosí", "Nuevo León", "Estado de México",
                  #"Baja California Sur", "Abierta", "Colima", "Extranjero", "Estado de México", "Durango",
                  #"San Luis Potosí", "Abierta", "Nuevo León", "Nayarit", "Extranjero", "Zacatecas",
                  #"Baja California", "Baja California", "Baja California", 
                  #"Baja California Sur", "Estado de México", "Ciudad de México", "Ciudad de México", "Ciudad de México")


#extract_replace_state <- function(string){
  #old_value <- gsub("[\\(\\)]", "", regmatches(string, gregexpr("\\(.*?\\)", string))[[1]])
  #new_value <- states_alias[which(states == old_value)]
  #new_value <- ifelse(identical(new_value, character(0)), yes="Ciudad de México", no=new_value)
#}

#archive$ciudad <- sapply(archive$ciudad, extract_replace_state)


### Exporting enrollment archive of all campus
# write.csv(archive, "../data/archive_2015_2020/archive_2015_2020.csv", row.names=FALSE)


###TEMPORAL
#write.csv(archive, "D:/Mariana/ACM-W/DataScience/archive_PRUEBA.csv", row.names=FALSE)

# TODO:
# Falta reemplazar los valores NULL por NA en promedio_up.
# Se debe identificar por qué estos alumnos no tienen promedio.
# Adicionalmente, estudiar cómo los NULL, NA y 0s son diferentes. 