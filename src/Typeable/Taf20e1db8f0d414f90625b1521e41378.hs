{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Taf20e1db8f0d414f90625b1521e41378 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Language = AAR{}
              | ABK{}
              | ACE{}
              | ACH{}
              | ADA{}
              | ADY{}
              | AFA{}
              | AFH{}
              | AFR{}
              | AIN{}
              | AKA{}
              | AKK{}
              | ALE{}
              | ALG{}
              | ALT{}
              | AMH{}
              | ANG{}
              | ANP{}
              | APA{}
              | ARA{}
              | ARC{}
              | ARG{}
              | ARN{}
              | ARP{}
              | ART{}
              | ARW{}
              | ASM{}
              | AST{}
              | ATH{}
              | AUS{}
              | AVA{}
              | AVE{}
              | AWA{}
              | AYM{}
              | AZE{}
              | BAD{}
              | BAI{}
              | BAK{}
              | BAL{}
              | BAM{}
              | BAN{}
              | BAS{}
              | BAT{}
              | BEJ{}
              | BEL{}
              | BEM{}
              | BEN{}
              | BER{}
              | BHO{}
              | BIH{}
              | BIK{}
              | BIN{}
              | BIS{}
              | BLA{}
              | BNT{}
              | BOD{}
              | BOS{}
              | BRA{}
              | BRE{}
              | BTK{}
              | BUA{}
              | BUG{}
              | BUL{}
              | BYN{}
              | CAD{}
              | CAI{}
              | CAR{}
              | CAT{}
              | CAU{}
              | CEB{}
              | CEL{}
              | CES{}
              | CHA{}
              | CHB{}
              | CHE{}
              | CHG{}
              | CHK{}
              | CHM{}
              | CHN{}
              | CHO{}
              | CHP{}
              | CHR{}
              | CHU{}
              | CHV{}
              | CHY{}
              | CMC{}
              | COP{}
              | COR{}
              | COS{}
              | CPE{}
              | CPF{}
              | CPP{}
              | CRE{}
              | CRH{}
              | CRP{}
              | CSB{}
              | CUS{}
              | CYM{}
              | DAK{}
              | DAN{}
              | DAR{}
              | DAY{}
              | DEL{}
              | DEN{}
              | DEU{}
              | DGR{}
              | DIN{}
              | DIV{}
              | DOI{}
              | DRA{}
              | DSB{}
              | DUA{}
              | DUM{}
              | DYU{}
              | DZO{}
              | EFI{}
              | EGY{}
              | EKA{}
              | ELL{}
              | ELX{}
              | ENG{}
              | ENM{}
              | EPO{}
              | EST{}
              | EUS{}
              | EWE{}
              | EWO{}
              | FAN{}
              | FAO{}
              | FAS{}
              | FAT{}
              | FIJ{}
              | FIL{}
              | FIN{}
              | FIU{}
              | FON{}
              | FRA{}
              | FRM{}
              | FRO{}
              | FRR{}
              | FRS{}
              | FRY{}
              | FUL{}
              | FUR{}
              | GAA{}
              | GAY{}
              | GBA{}
              | GEM{}
              | GEZ{}
              | GIL{}
              | GLA{}
              | GLE{}
              | GLG{}
              | GLV{}
              | GMH{}
              | GOH{}
              | GON{}
              | GOR{}
              | GOT{}
              | GRB{}
              | GRC{}
              | GRN{}
              | GSW{}
              | GUJ{}
              | GWI{}
              | HAI{}
              | HAT{}
              | HAU{}
              | HAW{}
              | HEB{}
              | HER{}
              | HIL{}
              | HIM{}
              | HIN{}
              | HIT{}
              | HMN{}
              | HMO{}
              | HRV{}
              | HSB{}
              | HUN{}
              | HUP{}
              | HYE{}
              | IBA{}
              | IBO{}
              | IDO{}
              | III{}
              | IJO{}
              | IKU{}
              | ILE{}
              | ILO{}
              | INA{}
              | INC{}
              | IND{}
              | INE{}
              | INH{}
              | IPK{}
              | IRA{}
              | IRO{}
              | ISL{}
              | ITA{}
              | JAV{}
              | JBO{}
              | JPN{}
              | JPR{}
              | JRB{}
              | KAA{}
              | KAB{}
              | KAC{}
              | KAL{}
              | KAM{}
              | KAN{}
              | KAR{}
              | KAS{}
              | KAT{}
              | KAU{}
              | KAW{}
              | KAZ{}
              | KBD{}
              | KHA{}
              | KHI{}
              | KHM{}
              | KHO{}
              | KIK{}
              | KIN{}
              | KIR{}
              | KMB{}
              | KOK{}
              | KOM{}
              | KON{}
              | KOR{}
              | KOS{}
              | KPE{}
              | KRC{}
              | KRL{}
              | KRO{}
              | KRU{}
              | KUA{}
              | KUM{}
              | KUR{}
              | KUT{}
              | LAD{}
              | LAH{}
              | LAM{}
              | LAO{}
              | LAT{}
              | LAV{}
              | LEZ{}
              | LIM{}
              | LIN{}
              | LIT{}
              | LOL{}
              | LOZ{}
              | LTZ{}
              | LUA{}
              | LUB{}
              | LUG{}
              | LUI{}
              | LUN{}
              | LUO{}
              | LUS{}
              | MAD{}
              | MAG{}
              | MAH{}
              | MAI{}
              | MAK{}
              | MAL{}
              | MAN{}
              | MAP{}
              | MAR{}
              | MAS{}
              | MDF{}
              | MDR{}
              | MEN{}
              | MGA{}
              | MIC{}
              | MIN{}
              | MIS{}
              | MKD{}
              | MKH{}
              | MLG{}
              | MLT{}
              | MNC{}
              | MNI{}
              | MNO{}
              | MOH{}
              | MON{}
              | MOS{}
              | MRI{}
              | MSA{}
              | MUL{}
              | MUN{}
              | MUS{}
              | MWL{}
              | MWR{}
              | MYA{}
              | MYN{}
              | MYV{}
              | NAH{}
              | NAI{}
              | NAP{}
              | NAU{}
              | NAV{}
              | NBL{}
              | NDE{}
              | NDO{}
              | NDS{}
              | NEP{}
              | NEW{}
              | NIA{}
              | NIC{}
              | NIU{}
              | NLD{}
              | NNO{}
              | NOB{}
              | NOG{}
              | NON{}
              | NOR{}
              | NQO{}
              | NSO{}
              | NUB{}
              | NWC{}
              | NYA{}
              | NYM{}
              | NYN{}
              | NYO{}
              | NZI{}
              | OCI{}
              | OJI{}
              | ORI{}
              | ORM{}
              | OSA{}
              | OSS{}
              | OTA{}
              | OTO{}
              | PAA{}
              | PAG{}
              | PAL{}
              | PAM{}
              | PAN{}
              | PAP{}
              | PAU{}
              | PEO{}
              | PHI{}
              | PHN{}
              | PLI{}
              | POL{}
              | PON{}
              | POR{}
              | PRA{}
              | PRO{}
              | PUS{}
              | QAA{}
              | QUE{}
              | RAJ{}
              | RAP{}
              | RAR{}
              | ROA{}
              | ROH{}
              | ROM{}
              | RON{}
              | RUN{}
              | RUP{}
              | RUS{}
              | SAD{}
              | SAG{}
              | SAH{}
              | SAI{}
              | SAL{}
              | SAM{}
              | SAN{}
              | SAS{}
              | SAT{}
              | SCN{}
              | SCO{}
              | SEL{}
              | SEM{}
              | SGA{}
              | SGN{}
              | SHN{}
              | SID{}
              | SIN{}
              | SIO{}
              | SIT{}
              | SLA{}
              | SLK{}
              | SLV{}
              | SMA{}
              | SME{}
              | SMI{}
              | SMJ{}
              | SMN{}
              | SMO{}
              | SMS{}
              | SNA{}
              | SND{}
              | SNK{}
              | SOG{}
              | SOM{}
              | SON{}
              | SOT{}
              | SPA{}
              | SQI{}
              | SRD{}
              | SRN{}
              | SRP{}
              | SRR{}
              | SSA{}
              | SSW{}
              | SUK{}
              | SUN{}
              | SUS{}
              | SUX{}
              | SWA{}
              | SWE{}
              | SYC{}
              | SYR{}
              | TAH{}
              | TAI{}
              | TAM{}
              | TAT{}
              | TEL{}
              | TEM{}
              | TER{}
              | TET{}
              | TGK{}
              | TGL{}
              | THA{}
              | TIG{}
              | TIR{}
              | TIV{}
              | TKL{}
              | TLH{}
              | TLI{}
              | TMH{}
              | TOG{}
              | TON{}
              | TPI{}
              | TSI{}
              | TSN{}
              | TSO{}
              | TUK{}
              | TUM{}
              | TUP{}
              | TUR{}
              | TUT{}
              | TVL{}
              | TWI{}
              | TYV{}
              | UDM{}
              | UGA{}
              | UIG{}
              | UKR{}
              | UMB{}
              | UND{}
              | URD{}
              | UZB{}
              | VAI{}
              | VEN{}
              | VIE{}
              | VOL{}
              | VOT{}
              | WAK{}
              | WAL{}
              | WAR{}
              | WAS{}
              | WEN{}
              | WLN{}
              | WOL{}
              | XAL{}
              | XHO{}
              | YAO{}
              | YAP{}
              | YID{}
              | YOR{}
              | YPK{}
              | ZAP{}
              | ZBL{}
              | ZEN{}
              | ZHA{}
              | ZHO{}
              | ZND{}
              | ZUL{}
              | ZUN{}
              | ZXX{}
              | ZZA{}
 
deriving instance Prelude.Eq Language
 
deriving instance Prelude.Ord Language
 
deriving instance Prelude.Show Language
 
instance Typeable.Internal.EBF.EBF Language where
        get
          = do index <- Data.Binary.Get.getWord16be
               case index of
                   0 -> return AAR
                   1 -> return ABK
                   2 -> return ACE
                   3 -> return ACH
                   4 -> return ADA
                   5 -> return ADY
                   6 -> return AFA
                   7 -> return AFH
                   8 -> return AFR
                   9 -> return AIN
                   10 -> return AKA
                   11 -> return AKK
                   12 -> return ALE
                   13 -> return ALG
                   14 -> return ALT
                   15 -> return AMH
                   16 -> return ANG
                   17 -> return ANP
                   18 -> return APA
                   19 -> return ARA
                   20 -> return ARC
                   21 -> return ARG
                   22 -> return ARN
                   23 -> return ARP
                   24 -> return ART
                   25 -> return ARW
                   26 -> return ASM
                   27 -> return AST
                   28 -> return ATH
                   29 -> return AUS
                   30 -> return AVA
                   31 -> return AVE
                   32 -> return AWA
                   33 -> return AYM
                   34 -> return AZE
                   35 -> return BAD
                   36 -> return BAI
                   37 -> return BAK
                   38 -> return BAL
                   39 -> return BAM
                   40 -> return BAN
                   41 -> return BAS
                   42 -> return BAT
                   43 -> return BEJ
                   44 -> return BEL
                   45 -> return BEM
                   46 -> return BEN
                   47 -> return BER
                   48 -> return BHO
                   49 -> return BIH
                   50 -> return BIK
                   51 -> return BIN
                   52 -> return BIS
                   53 -> return BLA
                   54 -> return BNT
                   55 -> return BOD
                   56 -> return BOS
                   57 -> return BRA
                   58 -> return BRE
                   59 -> return BTK
                   60 -> return BUA
                   61 -> return BUG
                   62 -> return BUL
                   63 -> return BYN
                   64 -> return CAD
                   65 -> return CAI
                   66 -> return CAR
                   67 -> return CAT
                   68 -> return CAU
                   69 -> return CEB
                   70 -> return CEL
                   71 -> return CES
                   72 -> return CHA
                   73 -> return CHB
                   74 -> return CHE
                   75 -> return CHG
                   76 -> return CHK
                   77 -> return CHM
                   78 -> return CHN
                   79 -> return CHO
                   80 -> return CHP
                   81 -> return CHR
                   82 -> return CHU
                   83 -> return CHV
                   84 -> return CHY
                   85 -> return CMC
                   86 -> return COP
                   87 -> return COR
                   88 -> return COS
                   89 -> return CPE
                   90 -> return CPF
                   91 -> return CPP
                   92 -> return CRE
                   93 -> return CRH
                   94 -> return CRP
                   95 -> return CSB
                   96 -> return CUS
                   97 -> return CYM
                   98 -> return DAK
                   99 -> return DAN
                   100 -> return DAR
                   101 -> return DAY
                   102 -> return DEL
                   103 -> return DEN
                   104 -> return DEU
                   105 -> return DGR
                   106 -> return DIN
                   107 -> return DIV
                   108 -> return DOI
                   109 -> return DRA
                   110 -> return DSB
                   111 -> return DUA
                   112 -> return DUM
                   113 -> return DYU
                   114 -> return DZO
                   115 -> return EFI
                   116 -> return EGY
                   117 -> return EKA
                   118 -> return ELL
                   119 -> return ELX
                   120 -> return ENG
                   121 -> return ENM
                   122 -> return EPO
                   123 -> return EST
                   124 -> return EUS
                   125 -> return EWE
                   126 -> return EWO
                   127 -> return FAN
                   128 -> return FAO
                   129 -> return FAS
                   130 -> return FAT
                   131 -> return FIJ
                   132 -> return FIL
                   133 -> return FIN
                   134 -> return FIU
                   135 -> return FON
                   136 -> return FRA
                   137 -> return FRM
                   138 -> return FRO
                   139 -> return FRR
                   140 -> return FRS
                   141 -> return FRY
                   142 -> return FUL
                   143 -> return FUR
                   144 -> return GAA
                   145 -> return GAY
                   146 -> return GBA
                   147 -> return GEM
                   148 -> return GEZ
                   149 -> return GIL
                   150 -> return GLA
                   151 -> return GLE
                   152 -> return GLG
                   153 -> return GLV
                   154 -> return GMH
                   155 -> return GOH
                   156 -> return GON
                   157 -> return GOR
                   158 -> return GOT
                   159 -> return GRB
                   160 -> return GRC
                   161 -> return GRN
                   162 -> return GSW
                   163 -> return GUJ
                   164 -> return GWI
                   165 -> return HAI
                   166 -> return HAT
                   167 -> return HAU
                   168 -> return HAW
                   169 -> return HEB
                   170 -> return HER
                   171 -> return HIL
                   172 -> return HIM
                   173 -> return HIN
                   174 -> return HIT
                   175 -> return HMN
                   176 -> return HMO
                   177 -> return HRV
                   178 -> return HSB
                   179 -> return HUN
                   180 -> return HUP
                   181 -> return HYE
                   182 -> return IBA
                   183 -> return IBO
                   184 -> return IDO
                   185 -> return III
                   186 -> return IJO
                   187 -> return IKU
                   188 -> return ILE
                   189 -> return ILO
                   190 -> return INA
                   191 -> return INC
                   192 -> return IND
                   193 -> return INE
                   194 -> return INH
                   195 -> return IPK
                   196 -> return IRA
                   197 -> return IRO
                   198 -> return ISL
                   199 -> return ITA
                   200 -> return JAV
                   201 -> return JBO
                   202 -> return JPN
                   203 -> return JPR
                   204 -> return JRB
                   205 -> return KAA
                   206 -> return KAB
                   207 -> return KAC
                   208 -> return KAL
                   209 -> return KAM
                   210 -> return KAN
                   211 -> return KAR
                   212 -> return KAS
                   213 -> return KAT
                   214 -> return KAU
                   215 -> return KAW
                   216 -> return KAZ
                   217 -> return KBD
                   218 -> return KHA
                   219 -> return KHI
                   220 -> return KHM
                   221 -> return KHO
                   222 -> return KIK
                   223 -> return KIN
                   224 -> return KIR
                   225 -> return KMB
                   226 -> return KOK
                   227 -> return KOM
                   228 -> return KON
                   229 -> return KOR
                   230 -> return KOS
                   231 -> return KPE
                   232 -> return KRC
                   233 -> return KRL
                   234 -> return KRO
                   235 -> return KRU
                   236 -> return KUA
                   237 -> return KUM
                   238 -> return KUR
                   239 -> return KUT
                   240 -> return LAD
                   241 -> return LAH
                   242 -> return LAM
                   243 -> return LAO
                   244 -> return LAT
                   245 -> return LAV
                   246 -> return LEZ
                   247 -> return LIM
                   248 -> return LIN
                   249 -> return LIT
                   250 -> return LOL
                   251 -> return LOZ
                   252 -> return LTZ
                   253 -> return LUA
                   254 -> return LUB
                   255 -> return LUG
                   256 -> return LUI
                   257 -> return LUN
                   258 -> return LUO
                   259 -> return LUS
                   260 -> return MAD
                   261 -> return MAG
                   262 -> return MAH
                   263 -> return MAI
                   264 -> return MAK
                   265 -> return MAL
                   266 -> return MAN
                   267 -> return MAP
                   268 -> return MAR
                   269 -> return MAS
                   270 -> return MDF
                   271 -> return MDR
                   272 -> return MEN
                   273 -> return MGA
                   274 -> return MIC
                   275 -> return MIN
                   276 -> return MIS
                   277 -> return MKD
                   278 -> return MKH
                   279 -> return MLG
                   280 -> return MLT
                   281 -> return MNC
                   282 -> return MNI
                   283 -> return MNO
                   284 -> return MOH
                   285 -> return MON
                   286 -> return MOS
                   287 -> return MRI
                   288 -> return MSA
                   289 -> return MUL
                   290 -> return MUN
                   291 -> return MUS
                   292 -> return MWL
                   293 -> return MWR
                   294 -> return MYA
                   295 -> return MYN
                   296 -> return MYV
                   297 -> return NAH
                   298 -> return NAI
                   299 -> return NAP
                   300 -> return NAU
                   301 -> return NAV
                   302 -> return NBL
                   303 -> return NDE
                   304 -> return NDO
                   305 -> return NDS
                   306 -> return NEP
                   307 -> return NEW
                   308 -> return NIA
                   309 -> return NIC
                   310 -> return NIU
                   311 -> return NLD
                   312 -> return NNO
                   313 -> return NOB
                   314 -> return NOG
                   315 -> return NON
                   316 -> return NOR
                   317 -> return NQO
                   318 -> return NSO
                   319 -> return NUB
                   320 -> return NWC
                   321 -> return NYA
                   322 -> return NYM
                   323 -> return NYN
                   324 -> return NYO
                   325 -> return NZI
                   326 -> return OCI
                   327 -> return OJI
                   328 -> return ORI
                   329 -> return ORM
                   330 -> return OSA
                   331 -> return OSS
                   332 -> return OTA
                   333 -> return OTO
                   334 -> return PAA
                   335 -> return PAG
                   336 -> return PAL
                   337 -> return PAM
                   338 -> return PAN
                   339 -> return PAP
                   340 -> return PAU
                   341 -> return PEO
                   342 -> return PHI
                   343 -> return PHN
                   344 -> return PLI
                   345 -> return POL
                   346 -> return PON
                   347 -> return POR
                   348 -> return PRA
                   349 -> return PRO
                   350 -> return PUS
                   351 -> return QAA
                   352 -> return QUE
                   353 -> return RAJ
                   354 -> return RAP
                   355 -> return RAR
                   356 -> return ROA
                   357 -> return ROH
                   358 -> return ROM
                   359 -> return RON
                   360 -> return RUN
                   361 -> return RUP
                   362 -> return RUS
                   363 -> return SAD
                   364 -> return SAG
                   365 -> return SAH
                   366 -> return SAI
                   367 -> return SAL
                   368 -> return SAM
                   369 -> return SAN
                   370 -> return SAS
                   371 -> return SAT
                   372 -> return SCN
                   373 -> return SCO
                   374 -> return SEL
                   375 -> return SEM
                   376 -> return SGA
                   377 -> return SGN
                   378 -> return SHN
                   379 -> return SID
                   380 -> return SIN
                   381 -> return SIO
                   382 -> return SIT
                   383 -> return SLA
                   384 -> return SLK
                   385 -> return SLV
                   386 -> return SMA
                   387 -> return SME
                   388 -> return SMI
                   389 -> return SMJ
                   390 -> return SMN
                   391 -> return SMO
                   392 -> return SMS
                   393 -> return SNA
                   394 -> return SND
                   395 -> return SNK
                   396 -> return SOG
                   397 -> return SOM
                   398 -> return SON
                   399 -> return SOT
                   400 -> return SPA
                   401 -> return SQI
                   402 -> return SRD
                   403 -> return SRN
                   404 -> return SRP
                   405 -> return SRR
                   406 -> return SSA
                   407 -> return SSW
                   408 -> return SUK
                   409 -> return SUN
                   410 -> return SUS
                   411 -> return SUX
                   412 -> return SWA
                   413 -> return SWE
                   414 -> return SYC
                   415 -> return SYR
                   416 -> return TAH
                   417 -> return TAI
                   418 -> return TAM
                   419 -> return TAT
                   420 -> return TEL
                   421 -> return TEM
                   422 -> return TER
                   423 -> return TET
                   424 -> return TGK
                   425 -> return TGL
                   426 -> return THA
                   427 -> return TIG
                   428 -> return TIR
                   429 -> return TIV
                   430 -> return TKL
                   431 -> return TLH
                   432 -> return TLI
                   433 -> return TMH
                   434 -> return TOG
                   435 -> return TON
                   436 -> return TPI
                   437 -> return TSI
                   438 -> return TSN
                   439 -> return TSO
                   440 -> return TUK
                   441 -> return TUM
                   442 -> return TUP
                   443 -> return TUR
                   444 -> return TUT
                   445 -> return TVL
                   446 -> return TWI
                   447 -> return TYV
                   448 -> return UDM
                   449 -> return UGA
                   450 -> return UIG
                   451 -> return UKR
                   452 -> return UMB
                   453 -> return UND
                   454 -> return URD
                   455 -> return UZB
                   456 -> return VAI
                   457 -> return VEN
                   458 -> return VIE
                   459 -> return VOL
                   460 -> return VOT
                   461 -> return WAK
                   462 -> return WAL
                   463 -> return WAR
                   464 -> return WAS
                   465 -> return WEN
                   466 -> return WLN
                   467 -> return WOL
                   468 -> return XAL
                   469 -> return XHO
                   470 -> return YAO
                   471 -> return YAP
                   472 -> return YID
                   473 -> return YOR
                   474 -> return YPK
                   475 -> return ZAP
                   476 -> return ZBL
                   477 -> return ZEN
                   478 -> return ZHA
                   479 -> return ZHO
                   480 -> return ZND
                   481 -> return ZUL
                   482 -> return ZUN
                   483 -> return ZXX
                   484 -> return ZZA
        put AAR = do Data.Binary.Put.putWord16be 0
        put ABK = do Data.Binary.Put.putWord16be 1
        put ACE = do Data.Binary.Put.putWord16be 2
        put ACH = do Data.Binary.Put.putWord16be 3
        put ADA = do Data.Binary.Put.putWord16be 4
        put ADY = do Data.Binary.Put.putWord16be 5
        put AFA = do Data.Binary.Put.putWord16be 6
        put AFH = do Data.Binary.Put.putWord16be 7
        put AFR = do Data.Binary.Put.putWord16be 8
        put AIN = do Data.Binary.Put.putWord16be 9
        put AKA = do Data.Binary.Put.putWord16be 10
        put AKK = do Data.Binary.Put.putWord16be 11
        put ALE = do Data.Binary.Put.putWord16be 12
        put ALG = do Data.Binary.Put.putWord16be 13
        put ALT = do Data.Binary.Put.putWord16be 14
        put AMH = do Data.Binary.Put.putWord16be 15
        put ANG = do Data.Binary.Put.putWord16be 16
        put ANP = do Data.Binary.Put.putWord16be 17
        put APA = do Data.Binary.Put.putWord16be 18
        put ARA = do Data.Binary.Put.putWord16be 19
        put ARC = do Data.Binary.Put.putWord16be 20
        put ARG = do Data.Binary.Put.putWord16be 21
        put ARN = do Data.Binary.Put.putWord16be 22
        put ARP = do Data.Binary.Put.putWord16be 23
        put ART = do Data.Binary.Put.putWord16be 24
        put ARW = do Data.Binary.Put.putWord16be 25
        put ASM = do Data.Binary.Put.putWord16be 26
        put AST = do Data.Binary.Put.putWord16be 27
        put ATH = do Data.Binary.Put.putWord16be 28
        put AUS = do Data.Binary.Put.putWord16be 29
        put AVA = do Data.Binary.Put.putWord16be 30
        put AVE = do Data.Binary.Put.putWord16be 31
        put AWA = do Data.Binary.Put.putWord16be 32
        put AYM = do Data.Binary.Put.putWord16be 33
        put AZE = do Data.Binary.Put.putWord16be 34
        put BAD = do Data.Binary.Put.putWord16be 35
        put BAI = do Data.Binary.Put.putWord16be 36
        put BAK = do Data.Binary.Put.putWord16be 37
        put BAL = do Data.Binary.Put.putWord16be 38
        put BAM = do Data.Binary.Put.putWord16be 39
        put BAN = do Data.Binary.Put.putWord16be 40
        put BAS = do Data.Binary.Put.putWord16be 41
        put BAT = do Data.Binary.Put.putWord16be 42
        put BEJ = do Data.Binary.Put.putWord16be 43
        put BEL = do Data.Binary.Put.putWord16be 44
        put BEM = do Data.Binary.Put.putWord16be 45
        put BEN = do Data.Binary.Put.putWord16be 46
        put BER = do Data.Binary.Put.putWord16be 47
        put BHO = do Data.Binary.Put.putWord16be 48
        put BIH = do Data.Binary.Put.putWord16be 49
        put BIK = do Data.Binary.Put.putWord16be 50
        put BIN = do Data.Binary.Put.putWord16be 51
        put BIS = do Data.Binary.Put.putWord16be 52
        put BLA = do Data.Binary.Put.putWord16be 53
        put BNT = do Data.Binary.Put.putWord16be 54
        put BOD = do Data.Binary.Put.putWord16be 55
        put BOS = do Data.Binary.Put.putWord16be 56
        put BRA = do Data.Binary.Put.putWord16be 57
        put BRE = do Data.Binary.Put.putWord16be 58
        put BTK = do Data.Binary.Put.putWord16be 59
        put BUA = do Data.Binary.Put.putWord16be 60
        put BUG = do Data.Binary.Put.putWord16be 61
        put BUL = do Data.Binary.Put.putWord16be 62
        put BYN = do Data.Binary.Put.putWord16be 63
        put CAD = do Data.Binary.Put.putWord16be 64
        put CAI = do Data.Binary.Put.putWord16be 65
        put CAR = do Data.Binary.Put.putWord16be 66
        put CAT = do Data.Binary.Put.putWord16be 67
        put CAU = do Data.Binary.Put.putWord16be 68
        put CEB = do Data.Binary.Put.putWord16be 69
        put CEL = do Data.Binary.Put.putWord16be 70
        put CES = do Data.Binary.Put.putWord16be 71
        put CHA = do Data.Binary.Put.putWord16be 72
        put CHB = do Data.Binary.Put.putWord16be 73
        put CHE = do Data.Binary.Put.putWord16be 74
        put CHG = do Data.Binary.Put.putWord16be 75
        put CHK = do Data.Binary.Put.putWord16be 76
        put CHM = do Data.Binary.Put.putWord16be 77
        put CHN = do Data.Binary.Put.putWord16be 78
        put CHO = do Data.Binary.Put.putWord16be 79
        put CHP = do Data.Binary.Put.putWord16be 80
        put CHR = do Data.Binary.Put.putWord16be 81
        put CHU = do Data.Binary.Put.putWord16be 82
        put CHV = do Data.Binary.Put.putWord16be 83
        put CHY = do Data.Binary.Put.putWord16be 84
        put CMC = do Data.Binary.Put.putWord16be 85
        put COP = do Data.Binary.Put.putWord16be 86
        put COR = do Data.Binary.Put.putWord16be 87
        put COS = do Data.Binary.Put.putWord16be 88
        put CPE = do Data.Binary.Put.putWord16be 89
        put CPF = do Data.Binary.Put.putWord16be 90
        put CPP = do Data.Binary.Put.putWord16be 91
        put CRE = do Data.Binary.Put.putWord16be 92
        put CRH = do Data.Binary.Put.putWord16be 93
        put CRP = do Data.Binary.Put.putWord16be 94
        put CSB = do Data.Binary.Put.putWord16be 95
        put CUS = do Data.Binary.Put.putWord16be 96
        put CYM = do Data.Binary.Put.putWord16be 97
        put DAK = do Data.Binary.Put.putWord16be 98
        put DAN = do Data.Binary.Put.putWord16be 99
        put DAR = do Data.Binary.Put.putWord16be 100
        put DAY = do Data.Binary.Put.putWord16be 101
        put DEL = do Data.Binary.Put.putWord16be 102
        put DEN = do Data.Binary.Put.putWord16be 103
        put DEU = do Data.Binary.Put.putWord16be 104
        put DGR = do Data.Binary.Put.putWord16be 105
        put DIN = do Data.Binary.Put.putWord16be 106
        put DIV = do Data.Binary.Put.putWord16be 107
        put DOI = do Data.Binary.Put.putWord16be 108
        put DRA = do Data.Binary.Put.putWord16be 109
        put DSB = do Data.Binary.Put.putWord16be 110
        put DUA = do Data.Binary.Put.putWord16be 111
        put DUM = do Data.Binary.Put.putWord16be 112
        put DYU = do Data.Binary.Put.putWord16be 113
        put DZO = do Data.Binary.Put.putWord16be 114
        put EFI = do Data.Binary.Put.putWord16be 115
        put EGY = do Data.Binary.Put.putWord16be 116
        put EKA = do Data.Binary.Put.putWord16be 117
        put ELL = do Data.Binary.Put.putWord16be 118
        put ELX = do Data.Binary.Put.putWord16be 119
        put ENG = do Data.Binary.Put.putWord16be 120
        put ENM = do Data.Binary.Put.putWord16be 121
        put EPO = do Data.Binary.Put.putWord16be 122
        put EST = do Data.Binary.Put.putWord16be 123
        put EUS = do Data.Binary.Put.putWord16be 124
        put EWE = do Data.Binary.Put.putWord16be 125
        put EWO = do Data.Binary.Put.putWord16be 126
        put FAN = do Data.Binary.Put.putWord16be 127
        put FAO = do Data.Binary.Put.putWord16be 128
        put FAS = do Data.Binary.Put.putWord16be 129
        put FAT = do Data.Binary.Put.putWord16be 130
        put FIJ = do Data.Binary.Put.putWord16be 131
        put FIL = do Data.Binary.Put.putWord16be 132
        put FIN = do Data.Binary.Put.putWord16be 133
        put FIU = do Data.Binary.Put.putWord16be 134
        put FON = do Data.Binary.Put.putWord16be 135
        put FRA = do Data.Binary.Put.putWord16be 136
        put FRM = do Data.Binary.Put.putWord16be 137
        put FRO = do Data.Binary.Put.putWord16be 138
        put FRR = do Data.Binary.Put.putWord16be 139
        put FRS = do Data.Binary.Put.putWord16be 140
        put FRY = do Data.Binary.Put.putWord16be 141
        put FUL = do Data.Binary.Put.putWord16be 142
        put FUR = do Data.Binary.Put.putWord16be 143
        put GAA = do Data.Binary.Put.putWord16be 144
        put GAY = do Data.Binary.Put.putWord16be 145
        put GBA = do Data.Binary.Put.putWord16be 146
        put GEM = do Data.Binary.Put.putWord16be 147
        put GEZ = do Data.Binary.Put.putWord16be 148
        put GIL = do Data.Binary.Put.putWord16be 149
        put GLA = do Data.Binary.Put.putWord16be 150
        put GLE = do Data.Binary.Put.putWord16be 151
        put GLG = do Data.Binary.Put.putWord16be 152
        put GLV = do Data.Binary.Put.putWord16be 153
        put GMH = do Data.Binary.Put.putWord16be 154
        put GOH = do Data.Binary.Put.putWord16be 155
        put GON = do Data.Binary.Put.putWord16be 156
        put GOR = do Data.Binary.Put.putWord16be 157
        put GOT = do Data.Binary.Put.putWord16be 158
        put GRB = do Data.Binary.Put.putWord16be 159
        put GRC = do Data.Binary.Put.putWord16be 160
        put GRN = do Data.Binary.Put.putWord16be 161
        put GSW = do Data.Binary.Put.putWord16be 162
        put GUJ = do Data.Binary.Put.putWord16be 163
        put GWI = do Data.Binary.Put.putWord16be 164
        put HAI = do Data.Binary.Put.putWord16be 165
        put HAT = do Data.Binary.Put.putWord16be 166
        put HAU = do Data.Binary.Put.putWord16be 167
        put HAW = do Data.Binary.Put.putWord16be 168
        put HEB = do Data.Binary.Put.putWord16be 169
        put HER = do Data.Binary.Put.putWord16be 170
        put HIL = do Data.Binary.Put.putWord16be 171
        put HIM = do Data.Binary.Put.putWord16be 172
        put HIN = do Data.Binary.Put.putWord16be 173
        put HIT = do Data.Binary.Put.putWord16be 174
        put HMN = do Data.Binary.Put.putWord16be 175
        put HMO = do Data.Binary.Put.putWord16be 176
        put HRV = do Data.Binary.Put.putWord16be 177
        put HSB = do Data.Binary.Put.putWord16be 178
        put HUN = do Data.Binary.Put.putWord16be 179
        put HUP = do Data.Binary.Put.putWord16be 180
        put HYE = do Data.Binary.Put.putWord16be 181
        put IBA = do Data.Binary.Put.putWord16be 182
        put IBO = do Data.Binary.Put.putWord16be 183
        put IDO = do Data.Binary.Put.putWord16be 184
        put III = do Data.Binary.Put.putWord16be 185
        put IJO = do Data.Binary.Put.putWord16be 186
        put IKU = do Data.Binary.Put.putWord16be 187
        put ILE = do Data.Binary.Put.putWord16be 188
        put ILO = do Data.Binary.Put.putWord16be 189
        put INA = do Data.Binary.Put.putWord16be 190
        put INC = do Data.Binary.Put.putWord16be 191
        put IND = do Data.Binary.Put.putWord16be 192
        put INE = do Data.Binary.Put.putWord16be 193
        put INH = do Data.Binary.Put.putWord16be 194
        put IPK = do Data.Binary.Put.putWord16be 195
        put IRA = do Data.Binary.Put.putWord16be 196
        put IRO = do Data.Binary.Put.putWord16be 197
        put ISL = do Data.Binary.Put.putWord16be 198
        put ITA = do Data.Binary.Put.putWord16be 199
        put JAV = do Data.Binary.Put.putWord16be 200
        put JBO = do Data.Binary.Put.putWord16be 201
        put JPN = do Data.Binary.Put.putWord16be 202
        put JPR = do Data.Binary.Put.putWord16be 203
        put JRB = do Data.Binary.Put.putWord16be 204
        put KAA = do Data.Binary.Put.putWord16be 205
        put KAB = do Data.Binary.Put.putWord16be 206
        put KAC = do Data.Binary.Put.putWord16be 207
        put KAL = do Data.Binary.Put.putWord16be 208
        put KAM = do Data.Binary.Put.putWord16be 209
        put KAN = do Data.Binary.Put.putWord16be 210
        put KAR = do Data.Binary.Put.putWord16be 211
        put KAS = do Data.Binary.Put.putWord16be 212
        put KAT = do Data.Binary.Put.putWord16be 213
        put KAU = do Data.Binary.Put.putWord16be 214
        put KAW = do Data.Binary.Put.putWord16be 215
        put KAZ = do Data.Binary.Put.putWord16be 216
        put KBD = do Data.Binary.Put.putWord16be 217
        put KHA = do Data.Binary.Put.putWord16be 218
        put KHI = do Data.Binary.Put.putWord16be 219
        put KHM = do Data.Binary.Put.putWord16be 220
        put KHO = do Data.Binary.Put.putWord16be 221
        put KIK = do Data.Binary.Put.putWord16be 222
        put KIN = do Data.Binary.Put.putWord16be 223
        put KIR = do Data.Binary.Put.putWord16be 224
        put KMB = do Data.Binary.Put.putWord16be 225
        put KOK = do Data.Binary.Put.putWord16be 226
        put KOM = do Data.Binary.Put.putWord16be 227
        put KON = do Data.Binary.Put.putWord16be 228
        put KOR = do Data.Binary.Put.putWord16be 229
        put KOS = do Data.Binary.Put.putWord16be 230
        put KPE = do Data.Binary.Put.putWord16be 231
        put KRC = do Data.Binary.Put.putWord16be 232
        put KRL = do Data.Binary.Put.putWord16be 233
        put KRO = do Data.Binary.Put.putWord16be 234
        put KRU = do Data.Binary.Put.putWord16be 235
        put KUA = do Data.Binary.Put.putWord16be 236
        put KUM = do Data.Binary.Put.putWord16be 237
        put KUR = do Data.Binary.Put.putWord16be 238
        put KUT = do Data.Binary.Put.putWord16be 239
        put LAD = do Data.Binary.Put.putWord16be 240
        put LAH = do Data.Binary.Put.putWord16be 241
        put LAM = do Data.Binary.Put.putWord16be 242
        put LAO = do Data.Binary.Put.putWord16be 243
        put LAT = do Data.Binary.Put.putWord16be 244
        put LAV = do Data.Binary.Put.putWord16be 245
        put LEZ = do Data.Binary.Put.putWord16be 246
        put LIM = do Data.Binary.Put.putWord16be 247
        put LIN = do Data.Binary.Put.putWord16be 248
        put LIT = do Data.Binary.Put.putWord16be 249
        put LOL = do Data.Binary.Put.putWord16be 250
        put LOZ = do Data.Binary.Put.putWord16be 251
        put LTZ = do Data.Binary.Put.putWord16be 252
        put LUA = do Data.Binary.Put.putWord16be 253
        put LUB = do Data.Binary.Put.putWord16be 254
        put LUG = do Data.Binary.Put.putWord16be 255
        put LUI = do Data.Binary.Put.putWord16be 256
        put LUN = do Data.Binary.Put.putWord16be 257
        put LUO = do Data.Binary.Put.putWord16be 258
        put LUS = do Data.Binary.Put.putWord16be 259
        put MAD = do Data.Binary.Put.putWord16be 260
        put MAG = do Data.Binary.Put.putWord16be 261
        put MAH = do Data.Binary.Put.putWord16be 262
        put MAI = do Data.Binary.Put.putWord16be 263
        put MAK = do Data.Binary.Put.putWord16be 264
        put MAL = do Data.Binary.Put.putWord16be 265
        put MAN = do Data.Binary.Put.putWord16be 266
        put MAP = do Data.Binary.Put.putWord16be 267
        put MAR = do Data.Binary.Put.putWord16be 268
        put MAS = do Data.Binary.Put.putWord16be 269
        put MDF = do Data.Binary.Put.putWord16be 270
        put MDR = do Data.Binary.Put.putWord16be 271
        put MEN = do Data.Binary.Put.putWord16be 272
        put MGA = do Data.Binary.Put.putWord16be 273
        put MIC = do Data.Binary.Put.putWord16be 274
        put MIN = do Data.Binary.Put.putWord16be 275
        put MIS = do Data.Binary.Put.putWord16be 276
        put MKD = do Data.Binary.Put.putWord16be 277
        put MKH = do Data.Binary.Put.putWord16be 278
        put MLG = do Data.Binary.Put.putWord16be 279
        put MLT = do Data.Binary.Put.putWord16be 280
        put MNC = do Data.Binary.Put.putWord16be 281
        put MNI = do Data.Binary.Put.putWord16be 282
        put MNO = do Data.Binary.Put.putWord16be 283
        put MOH = do Data.Binary.Put.putWord16be 284
        put MON = do Data.Binary.Put.putWord16be 285
        put MOS = do Data.Binary.Put.putWord16be 286
        put MRI = do Data.Binary.Put.putWord16be 287
        put MSA = do Data.Binary.Put.putWord16be 288
        put MUL = do Data.Binary.Put.putWord16be 289
        put MUN = do Data.Binary.Put.putWord16be 290
        put MUS = do Data.Binary.Put.putWord16be 291
        put MWL = do Data.Binary.Put.putWord16be 292
        put MWR = do Data.Binary.Put.putWord16be 293
        put MYA = do Data.Binary.Put.putWord16be 294
        put MYN = do Data.Binary.Put.putWord16be 295
        put MYV = do Data.Binary.Put.putWord16be 296
        put NAH = do Data.Binary.Put.putWord16be 297
        put NAI = do Data.Binary.Put.putWord16be 298
        put NAP = do Data.Binary.Put.putWord16be 299
        put NAU = do Data.Binary.Put.putWord16be 300
        put NAV = do Data.Binary.Put.putWord16be 301
        put NBL = do Data.Binary.Put.putWord16be 302
        put NDE = do Data.Binary.Put.putWord16be 303
        put NDO = do Data.Binary.Put.putWord16be 304
        put NDS = do Data.Binary.Put.putWord16be 305
        put NEP = do Data.Binary.Put.putWord16be 306
        put NEW = do Data.Binary.Put.putWord16be 307
        put NIA = do Data.Binary.Put.putWord16be 308
        put NIC = do Data.Binary.Put.putWord16be 309
        put NIU = do Data.Binary.Put.putWord16be 310
        put NLD = do Data.Binary.Put.putWord16be 311
        put NNO = do Data.Binary.Put.putWord16be 312
        put NOB = do Data.Binary.Put.putWord16be 313
        put NOG = do Data.Binary.Put.putWord16be 314
        put NON = do Data.Binary.Put.putWord16be 315
        put NOR = do Data.Binary.Put.putWord16be 316
        put NQO = do Data.Binary.Put.putWord16be 317
        put NSO = do Data.Binary.Put.putWord16be 318
        put NUB = do Data.Binary.Put.putWord16be 319
        put NWC = do Data.Binary.Put.putWord16be 320
        put NYA = do Data.Binary.Put.putWord16be 321
        put NYM = do Data.Binary.Put.putWord16be 322
        put NYN = do Data.Binary.Put.putWord16be 323
        put NYO = do Data.Binary.Put.putWord16be 324
        put NZI = do Data.Binary.Put.putWord16be 325
        put OCI = do Data.Binary.Put.putWord16be 326
        put OJI = do Data.Binary.Put.putWord16be 327
        put ORI = do Data.Binary.Put.putWord16be 328
        put ORM = do Data.Binary.Put.putWord16be 329
        put OSA = do Data.Binary.Put.putWord16be 330
        put OSS = do Data.Binary.Put.putWord16be 331
        put OTA = do Data.Binary.Put.putWord16be 332
        put OTO = do Data.Binary.Put.putWord16be 333
        put PAA = do Data.Binary.Put.putWord16be 334
        put PAG = do Data.Binary.Put.putWord16be 335
        put PAL = do Data.Binary.Put.putWord16be 336
        put PAM = do Data.Binary.Put.putWord16be 337
        put PAN = do Data.Binary.Put.putWord16be 338
        put PAP = do Data.Binary.Put.putWord16be 339
        put PAU = do Data.Binary.Put.putWord16be 340
        put PEO = do Data.Binary.Put.putWord16be 341
        put PHI = do Data.Binary.Put.putWord16be 342
        put PHN = do Data.Binary.Put.putWord16be 343
        put PLI = do Data.Binary.Put.putWord16be 344
        put POL = do Data.Binary.Put.putWord16be 345
        put PON = do Data.Binary.Put.putWord16be 346
        put POR = do Data.Binary.Put.putWord16be 347
        put PRA = do Data.Binary.Put.putWord16be 348
        put PRO = do Data.Binary.Put.putWord16be 349
        put PUS = do Data.Binary.Put.putWord16be 350
        put QAA = do Data.Binary.Put.putWord16be 351
        put QUE = do Data.Binary.Put.putWord16be 352
        put RAJ = do Data.Binary.Put.putWord16be 353
        put RAP = do Data.Binary.Put.putWord16be 354
        put RAR = do Data.Binary.Put.putWord16be 355
        put ROA = do Data.Binary.Put.putWord16be 356
        put ROH = do Data.Binary.Put.putWord16be 357
        put ROM = do Data.Binary.Put.putWord16be 358
        put RON = do Data.Binary.Put.putWord16be 359
        put RUN = do Data.Binary.Put.putWord16be 360
        put RUP = do Data.Binary.Put.putWord16be 361
        put RUS = do Data.Binary.Put.putWord16be 362
        put SAD = do Data.Binary.Put.putWord16be 363
        put SAG = do Data.Binary.Put.putWord16be 364
        put SAH = do Data.Binary.Put.putWord16be 365
        put SAI = do Data.Binary.Put.putWord16be 366
        put SAL = do Data.Binary.Put.putWord16be 367
        put SAM = do Data.Binary.Put.putWord16be 368
        put SAN = do Data.Binary.Put.putWord16be 369
        put SAS = do Data.Binary.Put.putWord16be 370
        put SAT = do Data.Binary.Put.putWord16be 371
        put SCN = do Data.Binary.Put.putWord16be 372
        put SCO = do Data.Binary.Put.putWord16be 373
        put SEL = do Data.Binary.Put.putWord16be 374
        put SEM = do Data.Binary.Put.putWord16be 375
        put SGA = do Data.Binary.Put.putWord16be 376
        put SGN = do Data.Binary.Put.putWord16be 377
        put SHN = do Data.Binary.Put.putWord16be 378
        put SID = do Data.Binary.Put.putWord16be 379
        put SIN = do Data.Binary.Put.putWord16be 380
        put SIO = do Data.Binary.Put.putWord16be 381
        put SIT = do Data.Binary.Put.putWord16be 382
        put SLA = do Data.Binary.Put.putWord16be 383
        put SLK = do Data.Binary.Put.putWord16be 384
        put SLV = do Data.Binary.Put.putWord16be 385
        put SMA = do Data.Binary.Put.putWord16be 386
        put SME = do Data.Binary.Put.putWord16be 387
        put SMI = do Data.Binary.Put.putWord16be 388
        put SMJ = do Data.Binary.Put.putWord16be 389
        put SMN = do Data.Binary.Put.putWord16be 390
        put SMO = do Data.Binary.Put.putWord16be 391
        put SMS = do Data.Binary.Put.putWord16be 392
        put SNA = do Data.Binary.Put.putWord16be 393
        put SND = do Data.Binary.Put.putWord16be 394
        put SNK = do Data.Binary.Put.putWord16be 395
        put SOG = do Data.Binary.Put.putWord16be 396
        put SOM = do Data.Binary.Put.putWord16be 397
        put SON = do Data.Binary.Put.putWord16be 398
        put SOT = do Data.Binary.Put.putWord16be 399
        put SPA = do Data.Binary.Put.putWord16be 400
        put SQI = do Data.Binary.Put.putWord16be 401
        put SRD = do Data.Binary.Put.putWord16be 402
        put SRN = do Data.Binary.Put.putWord16be 403
        put SRP = do Data.Binary.Put.putWord16be 404
        put SRR = do Data.Binary.Put.putWord16be 405
        put SSA = do Data.Binary.Put.putWord16be 406
        put SSW = do Data.Binary.Put.putWord16be 407
        put SUK = do Data.Binary.Put.putWord16be 408
        put SUN = do Data.Binary.Put.putWord16be 409
        put SUS = do Data.Binary.Put.putWord16be 410
        put SUX = do Data.Binary.Put.putWord16be 411
        put SWA = do Data.Binary.Put.putWord16be 412
        put SWE = do Data.Binary.Put.putWord16be 413
        put SYC = do Data.Binary.Put.putWord16be 414
        put SYR = do Data.Binary.Put.putWord16be 415
        put TAH = do Data.Binary.Put.putWord16be 416
        put TAI = do Data.Binary.Put.putWord16be 417
        put TAM = do Data.Binary.Put.putWord16be 418
        put TAT = do Data.Binary.Put.putWord16be 419
        put TEL = do Data.Binary.Put.putWord16be 420
        put TEM = do Data.Binary.Put.putWord16be 421
        put TER = do Data.Binary.Put.putWord16be 422
        put TET = do Data.Binary.Put.putWord16be 423
        put TGK = do Data.Binary.Put.putWord16be 424
        put TGL = do Data.Binary.Put.putWord16be 425
        put THA = do Data.Binary.Put.putWord16be 426
        put TIG = do Data.Binary.Put.putWord16be 427
        put TIR = do Data.Binary.Put.putWord16be 428
        put TIV = do Data.Binary.Put.putWord16be 429
        put TKL = do Data.Binary.Put.putWord16be 430
        put TLH = do Data.Binary.Put.putWord16be 431
        put TLI = do Data.Binary.Put.putWord16be 432
        put TMH = do Data.Binary.Put.putWord16be 433
        put TOG = do Data.Binary.Put.putWord16be 434
        put TON = do Data.Binary.Put.putWord16be 435
        put TPI = do Data.Binary.Put.putWord16be 436
        put TSI = do Data.Binary.Put.putWord16be 437
        put TSN = do Data.Binary.Put.putWord16be 438
        put TSO = do Data.Binary.Put.putWord16be 439
        put TUK = do Data.Binary.Put.putWord16be 440
        put TUM = do Data.Binary.Put.putWord16be 441
        put TUP = do Data.Binary.Put.putWord16be 442
        put TUR = do Data.Binary.Put.putWord16be 443
        put TUT = do Data.Binary.Put.putWord16be 444
        put TVL = do Data.Binary.Put.putWord16be 445
        put TWI = do Data.Binary.Put.putWord16be 446
        put TYV = do Data.Binary.Put.putWord16be 447
        put UDM = do Data.Binary.Put.putWord16be 448
        put UGA = do Data.Binary.Put.putWord16be 449
        put UIG = do Data.Binary.Put.putWord16be 450
        put UKR = do Data.Binary.Put.putWord16be 451
        put UMB = do Data.Binary.Put.putWord16be 452
        put UND = do Data.Binary.Put.putWord16be 453
        put URD = do Data.Binary.Put.putWord16be 454
        put UZB = do Data.Binary.Put.putWord16be 455
        put VAI = do Data.Binary.Put.putWord16be 456
        put VEN = do Data.Binary.Put.putWord16be 457
        put VIE = do Data.Binary.Put.putWord16be 458
        put VOL = do Data.Binary.Put.putWord16be 459
        put VOT = do Data.Binary.Put.putWord16be 460
        put WAK = do Data.Binary.Put.putWord16be 461
        put WAL = do Data.Binary.Put.putWord16be 462
        put WAR = do Data.Binary.Put.putWord16be 463
        put WAS = do Data.Binary.Put.putWord16be 464
        put WEN = do Data.Binary.Put.putWord16be 465
        put WLN = do Data.Binary.Put.putWord16be 466
        put WOL = do Data.Binary.Put.putWord16be 467
        put XAL = do Data.Binary.Put.putWord16be 468
        put XHO = do Data.Binary.Put.putWord16be 469
        put YAO = do Data.Binary.Put.putWord16be 470
        put YAP = do Data.Binary.Put.putWord16be 471
        put YID = do Data.Binary.Put.putWord16be 472
        put YOR = do Data.Binary.Put.putWord16be 473
        put YPK = do Data.Binary.Put.putWord16be 474
        put ZAP = do Data.Binary.Put.putWord16be 475
        put ZBL = do Data.Binary.Put.putWord16be 476
        put ZEN = do Data.Binary.Put.putWord16be 477
        put ZHA = do Data.Binary.Put.putWord16be 478
        put ZHO = do Data.Binary.Put.putWord16be 479
        put ZND = do Data.Binary.Put.putWord16be 480
        put ZUL = do Data.Binary.Put.putWord16be 481
        put ZUN = do Data.Binary.Put.putWord16be 482
        put ZXX = do Data.Binary.Put.putWord16be 483
        put ZZA = do Data.Binary.Put.putWord16be 484
 
deriving instance Prelude.Enum Language