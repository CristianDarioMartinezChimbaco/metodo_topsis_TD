      * 
      * 
      *                             Online Cobol Lang Compiler.
      *                 Code, Compile, Run and Debug Cobol Lang program online.
      * Write your code in this editor and press "Run" button to execute it.
      * 
      * 
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TOPSIS-RIM.
    
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT CSV-PESOS ASSIGN TO 'PESOS.csv'
        ORGANIZATION IS LINE SEQUENTIAL.
        SELECT CSV-PAGOS ASSIGN TO 'PAGOS.csv'
        ORGANIZATION IS LINE SEQUENTIAL.
        SELECT CSV-SOLUCIONES ASSIGN TO 'SOLUCIONES.csv'
        ORGANIZATION IS LINE SEQUENTIAL.
    
        DATA DIVISION.
        FILE SECTION.
        FD  CSV-PESOS.
        01  PESOS.
            05  CSV-RECORD       PIC X(1000).
        FD  CSV-PAGOS.
        01  PAGOS.
            05  CSV-RECORD       PIC X(1000). 
        FD  CSV-SOLUCIONES.
        01  SOLUCIONES.
            05  CSV-RECORD       PIC X(1000). 
        
        WORKING-STORAGE SECTION.
        01  EOF                  PIC 9 VALUE 0.
        01  COMA-POS             PIC 9(3) VALUE 0.
        01  RESTO                PIC X(1000).
        01  RESTO-POS            PIC 9(5) VALUE 1.
        01  NUMERO               PIC X(14).
        01  POSS                 PIC 9(2) VALUE 1.
        01  BAND                 PIC 9 VALUE 0.
        
        01  J                    PIC 9(2) VALUE 1.
        01  I                    PIC 9(2) VALUE 1.
        01  OPCION               PIC 9(1).
        
        01  PESOS                OCCURS 30 TIMES.
            05  PESO             PIC 9(1)V9(13). 
        01  MAXIMOS              OCCURS 30 TIMES.
            05  MAXIMO           PIC S9(7)V9(4).     
        01  SOLUCION-POSITIVA    OCCURS 30 TIMES.
            05  SOLUCION         PIC S9(2)V9(4). 
        01  SOLUCION-NEGATIVA    OCCURS 30 TIMES.
            05  SOLUCION         PIC S9(2)V9(4).
        01  POSITIVA             OCCURS 30 TIMES.
            05 EXTREMO           PIC S9(1)V9(4).
        01  NEGATIVA             OCCURS 30 TIMES.
            05 EXTREMO           PIC S9(1)V9(4).
        01  SUMA-SEP-P           OCCURS 30 TIMES.
            05 SUMA              PIC S9(1)V9(4).
        01  SUMA-SEP-N           OCCURS 30 TIMES.
            05 SUMA              PIC S9(1)V9(4).
        01  INDICE-RELACION      OCCURS 30 TIMES.
            05 INDICE            PIC S9(1)V9(4).
        01  RESULTADO            PIC S9(1)V9(4).
        01  ALTERNATIVA-RES      PIC 9(2).
        01  NUMERO-CRITERIOS     PIC 9(2).
        01  NUMERO-ALTERNATIVAS  PIC 9(2).
        
        01  MATRIZ-DECISION.
            05  FILA             OCCURS 30 TIMES.
                10  COLUMNA      OCCURS 30 TIMES.
                    15  ELEMENTO PIC S9(7)V9(4).
        01  MATRIZ-NORMALIZADA.
            05  FILA             OCCURS 30 TIMES.
                10  COLUMNA      OCCURS 30 TIMES.
                    15  ELEMENTO PIC S9(1)V9(4).
        01  MATRIZ-PONDERADA.
            05  FILA             OCCURS 30 TIMES.
                10  COLUMNA      OCCURS 30 TIMES.
                    15  ELEMENTO PIC S9(1)V9(4).
        01  SEPARACION-POSITIVA.
            05  FILA             OCCURS 30 TIMES.
                10  COLUMNA      OCCURS 30 TIMES.
                    15  ELEMENTO PIC S9(1)V9(4).
        01  SEPARACION-NEGATIVA.
            05  FILA             OCCURS 30 TIMES.
                10  COLUMNA      OCCURS 30 TIMES.
                    15  ELEMENTO PIC S9(1)V9(4).

        PROCEDURE DIVISION.
        MAIN-PROCEDURE.
        DISPLAY " *****************************************"
        DISPLAY "**   TTTTT   OOO   PPPP   SSSS III  SSS  **"
        DISPLAY "**     T    O   O  P   P S      I  S     **"
        DISPLAY "**     T    O   O  PPPP   SSS   I   SSS  **"
        DISPLAY "**     T    O   O  P         S  I      S **"
        DISPLAY "**     T     OOO   P      SSS  III SSSS  **"
        DISPLAY " *****************************************"
        DISPLAY "        .-~~-.-~-.".
        DISPLAY "        :         )".
        DISPLAY "  .~ ~ -.\       /.- ~~ .".
        DISPLAY "  >       `.   .'       <".
        DISPLAY " (         .- -.         )".
        DISPLAY "  `- -.-~  `- -'  ~-.- -'".
        DISPLAY "    (        :        )           _ _ .-:".
        DISPLAY "     ~--.    :    .--~        .-~  .-~  }".
        DISPLAY "         ~-.-^-.-~ \_      .~  .-~   .~".
        DISPLAY "                  \ \'     \ '_ _ -~".
        DISPLAY "                   `.`.    //".
        DISPLAY "          . - ~ ~-.__`.`-.//".
        DISPLAY "      .-~   . - ~  }~ ~ ~-.~-.".
        DISPLAY "    .' .-~      .-~       :/~-.~-./:".
        DISPLAY "   /_~_ _ . - ~                 ~-.~-._".
        DISPLAY "                                     ~-.<" 
        DISPLAY " "
        DISPLAY "SELECCIONE [1] LEER CSV [2] MANUALMENTE"
        ACCEPT OPCION
        EVALUATE OPCION
        WHEN 1
        PERFORM LEER-ARCHIVO
        WHEN 2
        PERFORM DEFINIR-LIMITES
        PERFORM TIPO-PESOS
        PERFORM DEFINIR-MATRIZ
        PERFORM DEFINIR-SOLUCIONES
        WHEN OTHER
        PERFORM MAIN-PROCEDURE
        END-EVALUATE
        PERFORM CALCULAR-MAXIMOS
        PERFORM CALCULAR-NORMALIZADA
        PERFORM CALCULAR-PONDERADA
        PERFORM CALCULAR-SOLUCIONES-POSITIVAS
        PERFORM CALCULAR-SOLUCIONES-NEGATIVAS
        PERFORM CAL-SEPARACION-P
        PERFORM CAL-SEPARACION-N
        PERFORM SUMA-POSITIVA
        PERFORM SUMA-NEGATIVA
        PERFORM CALCULAR-IR
        PERFORM MEJOR-DECISION
        STOP RUN.
          
        LEER-ARCHIVO.
        PERFORM LEER-PESOS
        PERFORM LEER-PAGOS
        PERFORM LEER-SOLUCIONES
        EXIT.
        
        LEER-PESOS.
        OPEN INPUT CSV-PESOS
        PERFORM UNTIL EOF = 1
        READ CSV-PESOS
        AT END
        MOVE 1 TO EOF
        NOT AT END
        PERFORM SEPARAR-PESOS
        END-READ
        END-PERFORM
        CLOSE CSV-PESOS
        MOVE 0 TO EOF
        EXIT.
        
        SEPARAR-PESOS.
        MOVE CSV-RECORD OF PESOS TO RESTO
        PERFORM UNTIL BAND = 1
        IF RESTO (RESTO-POS:1) = ',' 
        OR RESTO (RESTO-POS:1) = SPACE
        IF RESTO (RESTO-POS:1) = SPACE
        MOVE 1 TO BAND
        END-IF
        MOVE NUMERO TO PESO (J)
        MOVE SPACES TO NUMERO
        MOVE 1 TO POSS
        ADD 1 TO NUMERO-CRITERIOS
        ADD 1 TO RESTO-POS
        ADD 1 TO J
        ELSE 
        STRING RESTO (RESTO-POS:1) DELIMITED BY SIZE 
        INTO NUMERO 
        WITH POINTER POSS
        END-STRING
        ADD 1 TO RESTO-POS
        END-IF        
        END-PERFORM
        MOVE 1 TO POSS
        MOVE 1 TO RESTO-POS
        MOVE 1 TO J
        MOVE 0 TO BAND
        EXIT.
        
        LEER-PAGOS.
        OPEN INPUT CSV-PAGOS
        PERFORM UNTIL EOF = 1
        READ CSV-PAGOS
        AT END
        MOVE 1 TO EOF
        NOT AT END
        ADD 1 TO NUMERO-ALTERNATIVAS
        PERFORM SEPARAR-PAGOS
        ADD 1 TO I
        END-READ
        END-PERFORM
        CLOSE CSV-PAGOS
        MOVE 1 TO I
        MOVE 0 TO EOF
        EXIT. 
        
        SEPARAR-PAGOS.
        MOVE CSV-RECORD OF PAGOS TO RESTO
        PERFORM UNTIL BAND = 1
        IF RESTO (RESTO-POS:1) = ',' 
        OR RESTO (RESTO-POS:1) = SPACE
        IF RESTO (RESTO-POS:1) = SPACE
        MOVE 1 TO BAND
        END-IF
        MOVE NUMERO TO ELEMENTO OF MATRIZ-DECISION (I, J)
        MOVE SPACES TO NUMERO
        MOVE 1 TO POSS
        ADD 1 TO RESTO-POS
        ADD 1 TO J
        ELSE 
        STRING RESTO (RESTO-POS:1) DELIMITED BY SIZE 
        INTO NUMERO 
        WITH POINTER POSS
        END-STRING
        ADD 1 TO RESTO-POS
        END-IF        
        END-PERFORM
        MOVE 1 TO POSS
        MOVE 1 TO RESTO-POS
        MOVE 1 TO J
        MOVE 0 TO BAND
        EXIT.
        
        LEER-SOLUCIONES.
        OPEN INPUT CSV-SOLUCIONES
        PERFORM UNTIL EOF = 1
        READ CSV-SOLUCIONES
        AT END
        MOVE 1 TO EOF
        NOT AT END
        PERFORM SEPARAR-SOLUCIONES
        END-READ
        END-PERFORM
        CLOSE CSV-SOLUCIONES
        MOVE 0 TO EOF
        EXIT. 
      
        SEPARAR-SOLUCIONES.
        MOVE CSV-RECORD OF SOLUCIONES TO RESTO
        PERFORM UNTIL BAND = 1
        IF RESTO (RESTO-POS:1) = ',' 
        OR RESTO (RESTO-POS:1) = SPACE
        IF RESTO (RESTO-POS:1) = SPACE
        MOVE 1 TO BAND
        END-IF
        IF NUMERO = "BENEFICIO"
        MOVE 1 TO SOLUCION OF SOLUCION-POSITIVA (J)
        ELSE
        MOVE -1 TO SOLUCION OF SOLUCION-POSITIVA (J)
        END-IF
        MULTIPLY SOLUCION OF SOLUCION-POSITIVA (J)
        BY -1
        GIVING SOLUCION OF SOLUCION-NEGATIVA (J)
        END-MULTIPLY
        MOVE SPACES TO NUMERO
        MOVE 1 TO POSS
        ADD 1 TO RESTO-POS
        ADD 1 TO J
        ELSE 
        STRING RESTO (RESTO-POS:1) DELIMITED BY SIZE 
        INTO NUMERO 
        WITH POINTER POSS
        END-STRING
        ADD 1 TO RESTO-POS
        END-IF        
        END-PERFORM
        MOVE 1 TO POSS
        MOVE 1 TO RESTO-POS
        MOVE 1 TO J
        MOVE 0 TO BAND
        EXIT.
      
        DEFINIR-LIMITES.
        DISPLAY "INGRESE EL NUMERO DE CRITERIOS"
        ACCEPT NUMERO-CRITERIOS
        DISPLAY "INGRESE EL NUMERO DE ALTERNATIVAS"
        ACCEPT NUMERO-ALTERNATIVAS
        EXIT.
      
        TIPO-PESOS.
        DISPLAY "INGRESE SU OPCION" 
        DISPLAY "[1] ESTABLECER PESOS MANUALMENTE"
        DISPLAY "[2] CALCULAR PESOS CON LA FORMULA"
        DISPLAY "[3] LEER PESOS DEL ARCHIVO"
        ACCEPT OPCION
        EVALUATE OPCION
        WHEN 1
        PERFORM ESTABLECER-PESOS
        WHEN 2
        PERFORM CALCULAR-PESOS
        WHEN 3
        PERFORM LEER-PESOS
        WHEN OTHER
        DISPLAY "OPCION INVALIDA"
        PERFORM TIPO-PESOS
        END-EVALUATE
        EXIT.
      
        ESTABLECER-PESOS.
        PERFORM NUMERO-CRITERIOS TIMES
        DISPLAY "INGRESE EL PESO PARA CRITERIO "J
        ACCEPT PESO (J)
        ADD 1 TO J
        END-PERFORM
        MOVE 1 TO J
        EXIT.
        
        CALCULAR-PESOS.
        PERFORM NUMERO-CRITERIOS TIMES
        COMPUTE PESO (J) = (2 * 
        (NUMERO-CRITERIOS + 1 - J ) ) 
        / (NUMERO-CRITERIOS * 
        (NUMERO-CRITERIOS + 1))
        END-COMPUTE
        DISPLAY "PESO: "PESO (J) 
        ADD 1 TO J 
        END-PERFORM
        MOVE 1 TO J
        EXIT.
      
        DEFINIR-MATRIZ.
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        DISPLAY "INGRESE EL CRITERIO "J" PARA ALTERNATIVA "I
        ACCEPT ELEMENTO OF MATRIZ-DECISION (I, J)
        ADD 1 TO J
        END-PERFORM
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT.
        
        CALCULAR-MAXIMOS.
        DISPLAY "******MAXIMOS******"
        PERFORM NUMERO-CRITERIOS TIMES
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF MAXIMO (J) <= ELEMENTO OF MATRIZ-DECISION (I, J)
        MOVE ELEMENTO OF MATRIZ-DECISION (I, J) TO MAXIMO (J)
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY MAXIMO (J) " | " WITH NO ADVANCING
        MOVE 1 TO I
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        EXIT.
        
        CALCULAR-NORMALIZADA. 
        DISPLAY "******MATRIZ NORMALIZADA******"
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        DIVIDE ELEMENTO OF MATRIZ-DECISION (I, J) 
        BY MAXIMO (J) 
        GIVING ELEMENTO OF MATRIZ-NORMALIZADA (I, J)      
        END-DIVIDE
        DISPLAY ELEMENTO OF MATRIZ-NORMALIZADA (I, J) 
        " | "WITH NO ADVANCING
        END-DISPLAY
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT.
        
        CALCULAR-PONDERADA. 
        DISPLAY " ".
        DISPLAY "        ________".
        DISPLAY "    /--/        \\".
        DISPLAY "   |   \\______   |".
        DISPLAY "   \\ - ---^^- / /".
        DISPLAY "             ||/".
        DISPLAY "             |||".
        DISPLAY "           .:'':.".
        DISPLAY "     /^/^^\\/     \\".
        DISPLAY " 0___O_\\O_/       |".
        DISPLAY " |               /              O".
        DISPLAY " |       .._    /              //".
        DISPLAY "  \\ ____/   |  |              //".
        DISPLAY "            |  |             //".
        DISPLAY "     ^^^^^  |  |            ||".
        DISPLAY "  ^^^^^^^^^^|  \\         __ /|".
        DISPLAY " ^^^^^^   ^^|   \\       /     \\".
        DISPLAY " ^^^^^    __|    \\____/        |".
        DISPLAY "   ^^^    \\    /               |".
        DISPLAY "            \\-/          (_     \\".
        DISPLAY "             |  |\\__________\\   |".
        DISPLAY "            /|  |          \\ \\  |".
        DISPLAY "    _______| |  |     ______\\ \\  \\".
        DISPLAY "   /    ____/   |    /    ____/   \\".
        DISPLAY "   \\(_ /         \\   \\(_ /        |".
        DISPLAY "       \\_(____.../       \\_(_____/".
        DISPLAY "******MATRIZ PONDERADA******"              
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        MULTIPLY ELEMENTO OF MATRIZ-NORMALIZADA (I, J) 
        BY PESO (J) 
        GIVING ELEMENTO OF MATRIZ-PONDERADA (I, J)      
        END-MULTIPLY
        DISPLAY ELEMENTO OF MATRIZ-PONDERADA (I, J) 
        " | " WITH NO ADVANCING
        END-DISPLAY
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT.
        
        DEFINIR-SOLUCIONES.
        PERFORM NUMERO-CRITERIOS TIMES
        DISPLAY "EL CRITERIO " J " ES [1] BENEFICIO [-1] COSTO"
        ACCEPT SOLUCION OF SOLUCION-POSITIVA (J)
        MULTIPLY SOLUCION OF SOLUCION-POSITIVA (J)
        BY -1
        GIVING SOLUCION OF SOLUCION-NEGATIVA (J)
        END-MULTIPLY
        DISPLAY "SOLUCION-: "SOLUCION OF SOLUCION-NEGATIVA (J)
        ADD 1 TO J
        END-PERFORM
        MOVE 1 TO J
        EXIT.
      
        CALCULAR-SOLUCIONES-POSITIVAS.
        DISPLAY "******SOLUCIONES POSITIVAS******"
        PERFORM NUMERO-CRITERIOS TIMES
        IF SOLUCION OF SOLUCION-POSITIVA (J) = 1
        PERFORM MAXIMO-POSITIVAS
        ELSE
        PERFORM MINIMO-POSITIVAS
        END-IF
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        EXIT.
        
        CALCULAR-SOLUCIONES-NEGATIVAS.
        DISPLAY "******SOLUCIONES NEGATIVAS******"
        PERFORM NUMERO-CRITERIOS TIMES
        IF SOLUCION OF SOLUCION-NEGATIVA (J) = 1
        PERFORM MAXIMO-NEGATIVAS
        ELSE
        PERFORM MINIMO-NEGATIVAS
        END-IF
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        EXIT.
        
        MAXIMO-POSITIVAS.
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF EXTREMO OF POSITIVA (J) 
        <= ELEMENTO OF MATRIZ-PONDERADA (I, J)
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J) 
        TO EXTREMO OF POSITIVA (J)
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY EXTREMO OF POSITIVA (J) " | " WITH NO ADVANCING
        MOVE 1 TO I
        EXIT.
        
        MINIMO-POSITIVAS.
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J)
        TO EXTREMO OF POSITIVA (J)
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF EXTREMO OF POSITIVA (J) 
        >= ELEMENTO OF MATRIZ-PONDERADA (I, J)
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J) 
        TO EXTREMO OF POSITIVA (J)
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY EXTREMO OF POSITIVA (J) " | " WITH NO ADVANCING
        MOVE 1 TO I
        EXIT.          
      
        MAXIMO-NEGATIVAS.
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF EXTREMO OF NEGATIVA (J) 
        <= ELEMENTO OF MATRIZ-PONDERADA (I, J)
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J) 
        TO EXTREMO OF NEGATIVA (J)
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY EXTREMO OF NEGATIVA (J) " | " WITH NO ADVANCING
        MOVE 1 TO I
        EXIT.
        
        MINIMO-NEGATIVAS.
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J)
        TO EXTREMO OF NEGATIVA (J)
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF EXTREMO OF NEGATIVA (J) 
        >= ELEMENTO OF MATRIZ-PONDERADA (I, J)
        MOVE ELEMENTO OF MATRIZ-PONDERADA (I, J) 
        TO EXTREMO OF NEGATIVA (J)
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY EXTREMO OF NEGATIVA (J) " | " WITH NO ADVANCING
        MOVE 1 TO I
        EXIT. 
      
        CAL-SEPARACION-P.
        DISPLAY " ".
        DISPLAY "   #**#_#***#".
        DISPLAY "  #+++++++++#".
        DISPLAY "   +#++++++#+".
        DISPLAY "   + #++++#+".
        DISPLAY "   +  #++# +".
        DISPLAY "    +  #  +".
        DISPLAY "    +  #".
        DISPLAY "    + #".
        DISPLAY "    #".
        DISPLAY "   #    ###### ".
        DISPLAY "  ##   ##****##* ###".
        DISPLAY " ###   #********#***###".
        DISPLAY " ###  ##***#(0)#**#*#*#".
        DISPLAY " ####***#******#*#****#".
        DISPLAY "  ####*# ##****##*****#".
        DISPLAY "   **   #**************#".
        DISPLAY "         #*****#*****#*#".
        DISPLAY "     +   #****#*#****#*#".
        DISPLAY "    + +  #***#***#****#*".
        DISPLAY "    + +  #***#   #****#".
        DISPLAY "    + + ###*#    ##***##".
        DISPLAY " #########################".
        DISPLAY " __L____OOOO_V____V__EEEE_".
        DISPLAY " __L____O__O__V___V__E____".
        DISPLAY " __L____O__O__V__V___EEE__".
        DISPLAY " __L____O__O___V_V___E____".
        DISPLAY " __LLLL_OOOO____V____EEEE_".
        DISPLAY "##########################".
        DISPLAY "******SEPARACION POSITIVA******"
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        COMPUTE ELEMENTO OF SEPARACION-POSITIVA (I, J) =
        ((ELEMENTO OF MATRIZ-PONDERADA (I, J))-
        (EXTREMO OF POSITIVA (J))) ** 2
        END-COMPUTE
        DISPLAY ELEMENTO OF SEPARACION-POSITIVA (I, J) " | " 
        WITH NO ADVANCING
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT.
      
        CAL-SEPARACION-N.
        DISPLAY "******SEPARACION NEGATIVA******"
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        COMPUTE ELEMENTO OF SEPARACION-NEGATIVA (I, J) =
        ((ELEMENTO OF MATRIZ-PONDERADA (I, J))-
        (EXTREMO OF NEGATIVA (J))) ** 2
        END-COMPUTE
        DISPLAY ELEMENTO OF SEPARACION-NEGATIVA (I, J) " | " 
        WITH NO ADVANCING
        ADD 1 TO J
        END-PERFORM
        DISPLAY " "
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT.
            
        SUMA-POSITIVA.
        DISPLAY "*****SUMA POSITIVA*****" 
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        ADD ELEMENTO OF SEPARACION-POSITIVA (I, J) 
        TO SUMA OF SUMA-SEP-P (I)
        ADD 1 TO J
        END-PERFORM
        COMPUTE SUMA OF SUMA-SEP-P (I) = 
        FUNCTION SQRT(SUMA OF SUMA-SEP-P (I))
        END-COMPUTE
        DISPLAY SUMA OF SUMA-SEP-P (I) 
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT. 
        
        SUMA-NEGATIVA.
        DISPLAY "******SUMA NEGATIVA******"
        PERFORM NUMERO-ALTERNATIVAS TIMES
        PERFORM NUMERO-CRITERIOS TIMES
        ADD ELEMENTO OF SEPARACION-NEGATIVA (I, J) 
        TO SUMA OF SUMA-SEP-N (I)
        ADD 1 TO J
        END-PERFORM
        COMPUTE SUMA OF SUMA-SEP-N (I) = 
        FUNCTION SQRT(SUMA OF SUMA-SEP-N (I))
        END-COMPUTE
        DISPLAY SUMA OF SUMA-SEP-N (I) 
        MOVE 1 TO J
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT. 
        
        CALCULAR-IR.
        DISPLAY "*****INDICE RELATIVO*****"
        PERFORM NUMERO-ALTERNATIVAS TIMES
        COMPUTE INDICE (I) = (SUMA OF SUMA-SEP-N (I))
        /((SUMA OF SUMA-SEP-P (I))+(SUMA OF SUMA-SEP-N (I)))
        END-COMPUTE
        DISPLAY INDICE (I)
        ADD 1 TO I
        END-PERFORM
        MOVE 1 TO I
        EXIT. 
            
        MEJOR-DECISION.
        MOVE INDICE (1) TO RESULTADO
        PERFORM NUMERO-ALTERNATIVAS TIMES
        IF RESULTADO <= INDICE (I)
        MOVE INDICE (I) TO RESULTADO
        MOVE I TO ALTERNATIVA-RES
        END-IF
        ADD 1 TO I
        END-PERFORM
        DISPLAY "LA MEJOR ALTERNAYIVA ES LA " ALTERNATIVA-RES
        DISPLAY "CON UN IR DE: " RESULTADO
        EXIT.