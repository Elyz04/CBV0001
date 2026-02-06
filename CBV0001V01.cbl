000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.                            
000000 PROGRAM-ID.                     CBV0001V99.
000000 DATA                            DIVISION.
000000 WORKING-STORAGE                 SECTION.
000000*/-------------------------------------------------------------/*
000000*  ホスト変数                                                    
000000*/-------------------------------------------------------------/*     
000000 01 WS-VARIABLES.
000000    03 WS-DATA                   PIC X(05).
000000    03 WS-TEMP                   PIC X(05).
000000    03 WS-NUM-VALUE              PIC 9(05).
000000    03 WS-LEN                    PIC 9(02).
000000*/-------------------------------------------------------------/*
000000*  定数定義                                                      
000000*/-------------------------------------------------------------/*     
000000 01 CST-VARIABLES.
000000    03 CST-LOOP-FLG              PIC X VALUE 'N'.
000000*===============================================================*         
000000*====        ＰＲＯＣＥＤＵＲＥ　　 　　ＤＩＶＩＳＩＯＮ        ====*         
000000*===============================================================* 
000000 PROCEDURE                       DIVISION.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CTL-MAIN
000000* [ NOTE ] メイン処理（入力制御・VALIDATE 呼出）
000000*/-------------------------------------------------------------/*
000000 CBV0001-CTL-MAIN.
000000     PERFORM UNTIL CST-LOOP-FLG = 'Y'
000000         MOVE 'Y'                TO CST-LOOP-FLG
000000         DISPLAY '--------------------------------'
000000         DISPLAY 'INPUT NUMERIC FIELD (MAX 5 DIGIT)'
000000         ACCEPT WS-DATA
000000         PERFORM CBV0001-CAS-NUL
000000         IF CST-LOOP-FLG = 'Y'
000000             PERFORM CBV0001-CAS-LEN
000000         END-IF
000000         IF CST-LOOP-FLG = 'Y'
000000             PERFORM CBV0001-CAS-NUM
000000         END-IF
000000     END-PERFORM.
000000     DISPLAY '--------------------------------'
000000     DISPLAY 'PASS VALIDATION'
000000     DISPLAY 'NUMERIC VALUE = ' WS-NUM-VALUE
000000     STOP RUN.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-INIT-WS
000000* [ NOTE ] ワーク変数初期化（WS クリア処理）
000000*/-------------------------------------------------------------/*
000000 CBV0001-INIT-WS.
000000     INITIALIZE WS-VARIABLES.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-NUL
000000* [ CASE ] CASE-NUL
000000* [ NOTE ] NULL／BLANK チェック
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-NUL.
000000     IF WS-DATA = SPACES
000000         MOVE 'N' TO CST-LOOP-FLG
000000         DISPLAY 'ERROR: INPUT CANNOT BE BLANK'
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-LEN
000000* [ CASE ] CASE-LEN
000000* [ NOTE ] 桁数チェック（TRIM後 1～5 桁）
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-LEN.
000000     COMPUTE WS-LEN =
000000         FUNCTION LENGTH(
000000             FUNCTION TRIM(WS-DATA TRAILING)
000000         )
000000     IF WS-LEN < 1 OR WS-LEN > 5
000000         MOVE 'N' TO CST-LOOP-FLG
000000         DISPLAY 'ERROR: LENGTH MUST BE 1 TO 5'
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-NUM
000000* [ CASE ] CASE-NUM
000000* [ NOTE ] 数値チェック（0～9 のみ）
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-NUM.
000000     MOVE WS-DATA                TO WS-TEMP
000000     INSPECT WS-TEMP
000000         REPLACING ALL '0' BY SPACE
000000                   ALL '1' BY SPACE
000000                   ALL '2' BY SPACE
000000                   ALL '3' BY SPACE
000000                   ALL '4' BY SPACE
000000                   ALL '5' BY SPACE
000000                   ALL '6' BY SPACE
000000                   ALL '7' BY SPACE
000000                   ALL '8' BY SPACE
000000                   ALL '9' BY SPACE
000000     IF FUNCTION TRIM(WS-TEMP) = SPACES
000000         MOVE WS-DATA TO WS-NUM-VALUE
000000     ELSE
000000         MOVE 'N' TO CST-LOOP-FLG
000000         DISPLAY 'ERROR: INPUT IS NOT NUMERIC'
000000     END-IF.
000000     EXIT.
000000*===============================================================*         
000000*====           ＥＮＤ　 　ＯＦ　 　ＰＲＯＣＥＤＵＲＥ　       ====*         
000000*===============================================================*
000000***************************************************************** 
