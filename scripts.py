import pandas as pd

def get_proyects_data(df):
    # Create a new dataframe producto_esperado with the column 'ID_PROYECTO'
    producto_esperado = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('PRODUCTO_ESPERADO_')]]
    producto_esperado.columns = ['ID_PROYECTO'] + [col.replace('PRODUCTO_ESPERADO_', '') for col in producto_esperado.columns if col != 'ID_PROYECTO']

    dataframes = []

    for i in range(1, 7):
        temp_df = producto_esperado[['ID_PROYECTO', 
                      f'CATEGORIA_{i}', 
                      f'NOMBRE.{i-1}' if i > 1 else 'NOMBRE', 
                      f'PUNTUACION.{i-1}' if i > 1 else 'PUNTUACION']]
        temp_df.columns = ['ID_PROYECTO', 'CATEGORIA', 'NOMBRE', 'PUNTUACION']
        dataframes.append(temp_df)

    producto_esperado = pd.concat(dataframes, ignore_index=True)
    producto_esperado = producto_esperado.dropna(subset=['PUNTUACION', 'CATEGORIA', 'NOMBRE'])

    # Create a new dataframe adscripciones with the column 'ID_PROYECTO' and other relevant columns.
    adscripcion = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('ADSCRIPCION_')]]

    dataframes = []

    for i in range(32):
        suffix = f".{i}" if i > 0 else ""
        temp_df = adscripcion[['ID_PROYECTO', 
                      f'ADSCRIPCION_{i+1}', 
                      f'ADSCRIPCION_TIPO{suffix}', 
                      f'ADSCRIPCION_HORAS_SEMANALES{suffix}', 
                      f'ADSCRIPCION_NUMERO_MESES{suffix}',
                      f'ADSCRIPCION_TOTAL_HORAS{suffix}']]
        temp_df.columns = ['ID_PROYECTO', 'ADSCRIPCION', 'ADSCRIPCION_TIPO', 'ADSCRIPCION_HORAS_SEMANALES', 'ADSCRIPCION_NUMERO_MESES', 'ADSCRIPCION_TOTAL_HORAS']
        dataframes.append(temp_df)

    adscripcion = pd.concat(dataframes, ignore_index=True)

    proyectos_2017 = df[df.columns[:df.columns.get_loc('CUANTIA') + 1]]
    
    return proyectos_2017, producto_esperado, adscripcion

import pandas as pd

def get_data_proyectos(df):
    # Specify the columns related to the projects
    PROYECTO_cols = ['ID_PROYECTO', 'NOMBRE_PROYECTO', 'RESUMEN', 'OBJETIVO', 'ORIGEN', 
                     'DESCRIPCION_CONVOCATORIA', 'TIPO_PROYECTO', 'FECHA_INICIO', 'FECHA_FIN', 
                     'ESTADO', 'TOTAL_PRESUPUESTO_INTERNO', 'TOTAL_GASTOS_NOMINA', 
                     'TOTAL_OTRO_PRESUPUESTO', 'PRESUPUESTO_EXTERNO', 'PRESUPUESTO_TOTAL', 'CUANTIA']

    # Create a new dataframe that only contains those columns
    PROYECTO = df[PROYECTO_cols]

    return PROYECTO

def get_entidad_externa(df):
    # Create a new dataframe entidades_externas with the column 'ID_PROYECTO' and other relevant columns
    entidades_externas = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('ENTIDAD_EXTERNA_')]]

    # Determine the maximum number of 'ENTIDAD_EXTERNA_' groups
    num_groups = max(int(col.split('_')[-1]) for col in entidades_externas.columns if col.startswith('ENTIDAD_EXTERNA_'))

    dataframes = []

    for i in range(1, num_groups + 1):
        temp_df = entidades_externas[['ID_PROYECTO', f'ENTIDAD_EXTERNA_{i}']]
        temp_df.columns = ['ID_PROYECTO', 'ENTIDAD']
        dataframes.append(temp_df)

    entidades_externas = pd.concat(dataframes, ignore_index=True)
    entidades_externas = entidades_externas.dropna(subset=['ENTIDAD'])  # drop rows where 'ENTIDAD' is nan

    return entidades_externas



def get_productos(df):
    # Create a new dataframe productos with the column 'ID_PROYECTO' and other relevant columns
    productos = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('PRODUCTO_ESPERADO_')]]

    # Determine the maximum number of 'PRODUCTO_ESPERADO_' groups
    num_groups = max(int(col.split('_')[-1]) for col in productos.columns if col.startswith('PRODUCTO_ESPERADO_CATEGORIA_'))

    dataframes = []

    for i in range(1, num_groups + 1):  # dynamically determine range
        temp_df = productos[['ID_PROYECTO',
                             f'PRODUCTO_ESPERADO_CATEGORIA_{i}',
                             'PRODUCTO_ESPERADO_NOMBRE',
                             'PRODUCTO_ESPERADO_PUNTUACION']]
        temp_df.columns = ['ID_PROYECTO', 'CATEGORIA', 'NOMBRE', 'PUNTUACION']
        dataframes.append(temp_df)

    productos = pd.concat(dataframes, ignore_index=True)
    productos = productos.dropna(subset=['CATEGORIA', 'NOMBRE', 'PUNTUACION'])  # drop rows where any column is nan

    return productos


def get_grupos(df):
    # Define your renaming map
    rename_dict = {f'LINEA_INVESTIGACION': f'GRUPO_LINEA_INVESTIGACION_1'}

    # Generate renaming dictionary for rest of the columns
    for i in range(2, 7):  # Adjust the range if necessary
        old_name = f'LINEA_INVESTIGACION.{i-1}'  # decrease the index by 1, not 2
        new_name = f'GRUPO_LINEA_INVESTIGACION_{i}'
        rename_dict[old_name] = new_name

    # Rename the columns
    df = df.rename(columns=rename_dict)
    
    # Create a new dataframe grupos with the column 'ID_PROYECTO' and other relevant columns
    grupos = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('GRUPO_')]]

    # Determine the maximum number of 'GRUPO_' groups
    num_grupos = max(int(col.split('_')[-1]) for col in grupos.columns if col.startswith('GRUPO_INVESTIGACION_'))

    dataframes = []

    for i in range(1, num_grupos + 1):
        columns = ['ID_PROYECTO', f'GRUPO_INVESTIGACION_{i}', f'GRUPO_LINEA_INVESTIGACION_{i}']
        temp_df = grupos[columns]
        temp_df.columns = ['ID_PROYECTO', 'GRUPO', 'LINEA_INVESTIGACION']
        dataframes.append(temp_df)

    grupos = pd.concat(dataframes, ignore_index=True)
    grupos = grupos.dropna(subset=['GRUPO'])  # drop rows where 'GRUPO' is nan

    return grupos


def get_programas(df):
    # Create a new dataframe programas with the column 'ID_PROYECTO' and other relevant columns
    programas = df[['ID_PROYECTO'] + [col for col in df.columns if col.startswith('PROGRAMA_')]]

    # Determine the maximum number of 'PROGRAMA_' groups
    num_programas = max(int(col.split('_')[-1]) for col in programas.columns if col.startswith('PROGRAMA_') and '_UNIDAD' not in col and '_REGION' not in col)

    dataframes = []

    for i in range(1, num_programas + 1):
        if i == 1:  # for the first set, the column names are 'PROGRAMA_UNIDAD' and 'PROGRAMA_REGION'
            columns = ['ID_PROYECTO', 'PROGRAMA_1', 'PROGRAMA_UNIDAD', 'PROGRAMA_REGION']
            temp_df = programas[columns]
            temp_df.columns = ['ID_PROYECTO', 'PROGRAMA', 'UNIDAD', 'REGION']
        else:  # for other sets, the column names follow the pattern 'PROGRAMA_{i}_UNIDAD' and 'PROGRAMA_{i}_REGION'
            columns = ['ID_PROYECTO', f'PROGRAMA_{i}', f'PROGRAMA_UNIDAD.{i-1}', f'PROGRAMA_REGION.{i-1}']
            temp_df = programas[columns]
            temp_df.columns = ['ID_PROYECTO', 'PROGRAMA', 'UNIDAD', 'REGION']
        dataframes.append(temp_df)

    programas = pd.concat(dataframes, ignore_index=True)
    programas = programas.dropna(subset=['PROGRAMA'])  # drop rows where 'PROGRAMA' is nan

    return programas

def get_adscripciones(df):
    num_adscripciones = 13  # Now the number of adscripciones is fixed to 13
    dataframes = []
    for i in range(num_adscripciones):
        suffix = f".{i}" if i > 0 else ""
        temp_df = df[['ID_PROYECTO', 
                      f'ADSCRIPCION_{i+1}', 
                      f'ADSCRIPCION_TIPO{suffix}', 
                      f'ADSCRIPCION_HORAS_SEMANALES{suffix}', 
                      f'ADSCRIPCION_NUMERO_MESES{suffix}', 
                      f'ADSCRIPCION_TOTAL_HORAS{suffix}']]
        temp_df.columns = ['ID_PROYECTO', 'ADSCRIPCION', 'TIPO', 'HORAS_SEMANALES', 'NUMERO_MESES', 'TOTAL_HORAS']
        dataframes.append(temp_df)
    
    adscripciones = pd.concat(dataframes, ignore_index=True)
    adscripciones = adscripciones.dropna(subset=['ADSCRIPCION'])  # drop rows where 'ADSCRIPCION' is nan

    return adscripciones