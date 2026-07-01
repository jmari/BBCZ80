def generar_tabla(nombre, max_val):
    print(f"; --- Tabla de escalado 1024 -> {max_val+1} ---")
    print(f"PUBLIC {nombre}")  # Exportamos la etiqueta por si la llamas desde C u otro módulo
    print(f"{nombre}:")
    
    for i in range(0, 1180, 16):
        # Calculamos 16 valores por línea y redondeamos correctamente
        valores = [str(round((j * max_val) / 1179)) for j in range(i, i+16)]
        print("    defb " + ", ".join(valores))
    print("")

#vGenerar ambas tablas (192 y 212)
#generar_tabla("lut_scale_192", 191)
#generar_tabla("lut_scale_212", 211)
generar_tabla("lut_scale_256", 255)