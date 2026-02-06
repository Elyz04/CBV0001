000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.                            
000000 PROGRAM-ID.                     CBV0001V99.
       DATA                            DIVISION.
       WORKING-STORAGE                 SECTION.
000000*/-------------------------------------------------------------/*
000000*  ホスト変数                                                    
000000*/-------------------------------------------------------------/*     
       01 WS-VARIABLES.
          03 WS-DATA                   PIC X(05).
          03 WS-TEMP                   PIC X(05).
          03 WS-NUM-VALUE              PIC 9(05).
          03 WS-LEN                    PIC 9(02).
000000*/-------------------------------------------------------------/*
000000*  定数定義                                                      
000000*/-------------------------------------------------------------/*     
       01 CST-VARIABLES.
          03 CST-LOOP-FLG              PIC X VALUE 'N'.
       PROCEDURE                       DIVISION.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CTL-MAIN
000000* [ NOTE ] メイン処理（入力制御・VALIDATE 呼出）
000000*/-------------------------------------------------------------/*
       CBV0001-CTL-MAIN.
           PERFORM UNTIL CST-LOOP-FLG = 'Y'
               MOVE 'Y'                 TO CST-LOOP-FLG
               DISPLAY '--------------------------------'
               DISPLAY 'INPUT NUMERIC FIELD (MAX 5 DIGIT)'
               ACCEPT WS-DATA
               PERFORM CBV0001-CAS-NUL
               IF CST-LOOP-FLG = 'Y'
                   PERFORM CBV0001-CAS-LEN
               END-IF
               IF CST-LOOP-FLG = 'Y'
                   PERFORM CBV0001-CAS-NUM
               END-IF
           END-PERFORM
           DISPLAY '--------------------------------'
           DISPLAY 'PASS VALIDATION'
           DISPLAY 'NUMERIC VALUE = ' WS-NUM-VALUE
           STOP RUN.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-INIT-WS
000000* [NOTE  ] ワーク変数初期化（WS クリア処理）
000000*/-------------------------------------------------------------/*
       CBV0001-INIT-WS.
           INITIALIZE WS-VARIABLES.
           EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-NUL
000000* [ CASE ] CASE-NUL
000000* [ NOTE ] NULL／BLANK チェック
000000*/-------------------------------------------------------------/*
       CBV0001-CAS-NUL.
           IF WS-DATA = SPACES
               MOVE 'N' TO CST-LOOP-FLG
               DISPLAY 'ERROR: INPUT CANNOT BE BLANK'
           END-IF.
           EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-LEN
000000* [ CASE ] CASE-LEN
000000* [ NOTE ] 桁数チェック（TRIM後 1～5 桁）
000000*/-------------------------------------------------------------/*
       CBV0001-CAS-LEN.
           COMPUTE WS-LEN =
               FUNCTION LENGTH(
                   FUNCTION TRIM(WS-DATA TRAILING)
               )

           IF WS-LEN < 1 OR WS-LEN > 5
               MOVE 'N' TO CST-LOOP-FLG
               DISPLAY 'ERROR: LENGTH MUST BE 1 TO 5'
           END-IF.
           EXIT.
000000*/-------------------------------------------------------------/*
000000* [MODULE] CBV0001-CAS-NUM
000000* [ CASE ] CASE-NUM
000000* [ NOTE ] 数値チェック（0～9 のみ）
000000*/-------------------------------------------------------------/*
       CBV0001-CAS-NUM.
           MOVE WS-DATA TO WS-TEMP
           INSPECT WS-TEMP
               REPLACING ALL '0' BY SPACE
                         ALL '1' BY SPACE
                         ALL '2' BY SPACE
                         ALL '3' BY SPACE
                         ALL '4' BY SPACE
                         ALL '5' BY SPACE
                         ALL '6' BY SPACE
                         ALL '7' BY SPACE
                         ALL '8' BY SPACE
                         ALL '9' BY SPACE
           IF FUNCTION TRIM(WS-TEMP) = SPACES
               MOVE WS-DATA TO WS-NUM-VALUE
           ELSE
               MOVE 'N' TO CST-LOOP-FLG
               DISPLAY 'ERROR: INPUT IS NOT NUMERIC'
           END-IF.
           EXIT.
