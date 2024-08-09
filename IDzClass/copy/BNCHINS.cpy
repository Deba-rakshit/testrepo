       01  PATIENT-INSURANCE.
           05  INS-COMPANY-PRIMARY.
               10  PATIENT-ID              PIC X(6).
               10  INS-COMPANY-PRIMARY-ID  PIC X(8).
               10  CARRIER-NAME            PIC X(30).
               10  CARRIER-PHONE           PIC X(10).
               10  CARRIER-FAX             PIC X(10).
               10  INSURED-NAME            PIC X(30).
               10  INSURED-GENDER          PIC X(01).
                   88  FEMALE          VALUE "F".
                   88  MALE            VALUE "M".
                   88  NOT-PROVIDED    VALUE "N".
                   88 VALID-GENDER
                       VALUES ARE "F", "M", "N".
               10  PATIENT-RELATIONSHIP    PIC X(02).
                   88  SPOUSE          VALUE "SP".
                   88  SELF-REL        VALUE "SE".
                   88  CHILD           VALUE "CH".
                   88  OTHER-REL       VALUE "OT".
                   88 VALID-RELS
                       VALUES ARE "SP", "SE", "SP", "OT".
               10  INS-IDENT-NBR           PIC X(20).
               10  GROUP-NBR               PIC X(10).
               10  ADDRESS-CLAIMS.
                   15  STREET              PIC X(30).
                   15  CITY                PIC X(20).
                   15  STATE               PIC X(02).
                   15  ZIP                 PIC X(9).
               10  RETIRED-IND    PIC X(01).
                   88 RETIRED          VALUE "Y".
                   88 NOT-RETIRED      VALUE "N".
                   88 VALID-RET-IND  VALUES ARE "Y", "N".
           05  INS-COMPANY-SECONDARY.
               10  CARRIER-ID              PIC X(8).
               10  CARRIER-NAME            PIC X(30).
               10  CARRIER-PHONE           PIC X(10).
               10  CARRIER-FAX             PIC X(10).
               10  INSUREDS-NAME           PIC X(30).
               10  PATIENT-RELATIONSHIP    PIC X(02).
                   88  SPOUSE          VALUE "SP".
                   88  SELF-REL        VALUE "SE".
                   88  CHILD           VALUE "CH".
                   88  OTHER-REL       VALUE "OT".
                   88 VALID-RELS
                       VALUES ARE "SP", "SE", "SP", "OT".
               10  INS-IDENT-NBR           PIC X(20).
               10  GROUP-NBR               PIC X(10).
               10  ADDRESS-CLAIMS.
                   15  STREET              PIC X(30).
                   15  CITY                PIC X(20).
                   15  STATE               PIC X(02).
                   15  ZIP                 PIC X(9).
           05  BENEFIT-ASSIGNMENT-DETAILS.
               10  MEDICARE-BENEFICIARY    PIC X(30).
               10  MEDICARE-CLAIM-NBR      PIC X(15).
               10  COMMERCIAL-BENEFICIARY  PIC X(30).
               10  COMMERCIAL-CLAIM-NBR    PIC X(15).
           05  PAT-INSURANCE-COMMENTS      PIC X(100).
           05  FILLER                      PIC X(142).