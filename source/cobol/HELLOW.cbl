       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOW.
      *////////////////////////////////////////////////////////////-*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      *////////////////////////////////////////////////////////////-*
       WORKING-STORAGE SECTION.
       77  FS-FILER      PIC XX    VALUE ZEROS.
       77  RETCODE       PIC S9(9) COMP VALUE +0.
       77  CURRENT-TIME  PIC 9(8).
       77  WS-VAR1       PIC X(4) VALUE '0060'.
       77  WS-VAR2       PIC X(5) VALUE SPACES.
       77  WS-EXAMPLE2   PIC XX.
       77  WS-TEXT       PIC X(10) VALUE 'HELLOW    '.
      *////////////////////////////////////////////////////////////-*
       PROCEDURE DIVISION.
      *////////////////////////////////////////////////////////////-*
       MAIN  SECTION.

            DISPLAY WS-TEXT ' Prisma HOALLLLLAAAAA'.

            GOBACK.

       END-MAIN. EXIT.
      *////////////////////////////////////////////////////////////-*
      *////////////////////////////////////////////////////////////-*
