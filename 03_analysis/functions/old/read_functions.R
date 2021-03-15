###############################################
# Funcao para leitura de dados do acelerometro
# Retorna dataframe com odba calculado de acordo com os parametros fornecidos
# As coluna de dados são 'timestamp' e 'odba'
###############################################
read_to_odba = function(file,
                        time_window = 5,
                        hz = 10) {
    require(data.table) # para leitura mais rápida de arquivos grandes
    require(lubridate) # manipulação de datas
    require(dplyr)
    
    # De uma só vez o comando a seguir lê o arquivo e calcula o odba sem salvar um objeto no R
    x = fread(file = file, header = TRUE, sep = "auto", fill = TRUE) %>%
    subtract_gravity(time_window = 3, hz = hz) %>%
    odba(time_window = time_window, hz = hz)
    
    # Converte coluna 'timestamp' para POSIXct
    x$timestamp = parse_date_time(x = x$timestamp, order = "dmYHMs")
    
    return(x) # Retorna dataframe
}

###############################################
# Funcao para leitura de dados do acelerometro
# Retorna dataframe com odba calculado de acordo com os parametros fornecidos
# As coluna de dados são 'timestamp' e 'odba'
###############################################
read_to_vedba = function(file,
                         interval = 5,
                         hz = 10,
                         sensor.id = FALSE) {
    require(data.table) # para leitura mais rápida de arquivos grandes
    require(lubridate) # manipulação de datas
    require(dplyr) # need to run every time you start R and want to use %>%
    
    # De uma só vez o comando a seguir lê o arquivo e calcula o odba sem salvar um objeto no R
    x = fread(file = file, header = TRUE, sep = "auto", fill = TRUE) %>%
    subtract_gravity(time_window = 3, hz = hz) %>%
    vedba(time_window = time_window, hz = hz)
    
    return(x) # Retorna dataframe
}

###############################################
# Funcao para leitura de dados do lightlogger
# Retorna dataframe com dados brutos
# As colunas de dados são renomeadas para 'timestamp' e 'lux'
###############################################
read_lightlogger = function(file, ...) {
    require(data.table) # para leitura mais rápida de arquivos grandes
    
    # Esse bloco lê o arquivo de dados, renomeia as colunas e converte 'timestamp' para POSIX
    x = fread(
    file = file,
    sep = "\t",
    header = TRUE,
    skip = 19
    )
    names(x) = c("datetime", "lux")
    
    return(x) # Retorna dataframe
}


