      MODULE VARIABLE
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     THE MODULE IS USED TO DEFINE THE VARIABLES
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  20/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.   Syear              INTEGER             -                Simulation Start Year
C 2.   Eyear              INTEGER             -                Simulation End Year
C 3.   Sday               INTEGER             -                Simulation Start Day
C 4.   Eday               INTEGER             -                Simulation End Day
C 5.   Smon               INTEGER             -                Simulation Start Month
C 6.   Emon               INTEGER             -                Similation End Month
C 7.   Jday               INTEGER             -                Julian Day of the year
C 8.   Nsoil              INTEGER             -                Number of soil layer
C 9.   Bdsoil             REAL               Kg/m3             Bulk Density of Soil
C 10.  Pdsoil             REAL               Kg/m3             Particle Density of soil
C 11.  Ksat               REAL               m/sec             Saturate Hydralic Conductivity of Soil
C 12.  Fc                 REAL               m3/m3             Field Capacity of the Soil
C 13.  Pwp                REAL               m3/m3             Permanent wilting point of the soil
C 14.  Ph                 REAL                -                Ph of the soil
C 15.  Psc                REAL                -                Phosphorus Sorption Coefficient
C 16.  Clay               REAL                %                Clay percentage of soil
C 17.  Sand               REAL                %                Sand percentage of soil
C 18.  Silt               REAL                %                Soil percentage of soil
C 19.  Orgm               REAL                %                Soil organic matter
C 20.  Labp               REAL              Kg/ha              Soil labile P
C 21.  Actp               REAL              Kg/ha              Soil Active P
C 22.  Stabip             REAL              Kg/ha              Soil Stable inorganic P
C 23.  Stabop             REAL              Kg/ha              Soil Stable organic P
C 24.  Frsop              REAL              Kg/ha              Soil Fresh organic P
C 25.  Manwip             REAL              Kg                 Manure Water Extractable Inorganic P
C 26.  Manwop             REAL              Kg                 Manure Water Extractable Organic P
C 27.  Mansop             REAL              Kg                 Manure Stable Organic P
C 28.  Mansip             REAL              Kg                 Manure Stable Inorganic P
C 29.  Crpres             REAL              Kg/ha              Crop Residue of the soil surface
C 30.  Area               REAL              ha                 Area of the field
C 31.  Tsoil              REAL              m                  Thickness of the soil layer
C 32.  Dsoil              REAL              m                  Depth of bottom of the soil layer from GL     
C 33.  Psoil              REAL              m3/m3              Porosity of the soil layer
C 34.  Orgc               REAL              %                  Soil organic carbon
C 35.  Smc                REAL              m3/m3              Soil Mositure Content
C 36.  Year               INTEGER           -                  Calender Year
C 37.  Month              INTEGER           -                  Calender Month
C 38.  Day                INTEGER           -                  Calender Day
C 39.  FDay               INTEGER           -                  Fertilizer Application Day
C 40.  Fmon               INTEGER           -                  Fertilizer Application Month
C 41.  Fyear              INTEGER           -                  Fertilizer Application Year
C 42.  Fertp              REAL              kg                 P Mass in Applied Fertilizer
C 43.  Fertd              REAL              m                  Depth of Fertilizer Application
C 44.  Flefts             REAL              %                  Percentage of Fertilizer left on surface
C 45.  Nfert              INTEGER           -                  Total number of fertilizer applied with in 
C                                                              the whole simulation period
C 46.  Avfrtp             REAL              Kg                 Fertilizer P Available readily directly after
C                                                              fertilizer application
C 47.  Tdavfertp           REAL             Kg                 Total daily fertilizer P Available readily directly after
C                                                              fertilizer application
C 48.  Daysfert           REAL           -                  Days since fertilizer applied
C 49.  Resfertp           INTEGER           Kg                 Residual Fertilizer P available not readily
C                                                              directlly after the fertilzser application.
C 50.  Slnf               INTEGER           -                  Seraial No of The Ferilizer Applied
C 51.  Scov               INTEGER           -                  Type of Soil Cover
C 52.  Fertpfr             REAL              -                 Fraction of Fertilizer P Remaining on a partilular day
C                                                              after the fertilizer application
C 53.  Fertpsorp           REAL             Kg                 Fertilizer P Sorped
C 54.  Appdayavfertp       REAL             Kg                 Application Day Available Fertilizer P
C 55.  Appdayresfertp      REAL             Kg                 Application Day Residual Fertilizer P
C 56.  Dummy1              REAL             Kg                 Dummy Veriable
C 56.  Dailyfertpsop       REAL             Kg                 Daily Fertilizer P Sorped
C 57.  Norain              INTEGER          -                  Number of daily rainfall event after fertilizer application  
C 58.  Fertprelease        REAL             Kg                 Daily Fertilizer P Release dure to rainfall
C 59.  Fertpcrunoff        REAL             Kg/m3              Fertilizer P Concentration in Runoff
C 60.  Fertpmrunoff        REAL             Kg                 Fertilizer P Mass in Runoff
C 61.  Nman                INTEGER          -                  Number of Manure Application within the whole simulation period
C 62.  Mday                INTEGER          -                  Manure Application Day
C 63.  Mmon                INTEGER          -                  Manure Application Month
C 64.  Myear               INTEGER          -                  Manure Application Year
C 65.  Manmass             REAL             Kg                 Manure Maass
C 66.  Manpmass            REAL             Kg                 Manure P mass
C 67.  Mantpper            REAL             %                  Manure Total P Percentage
C 68.  Manweipper          REAL             %                  Manure Water Extractable inorganic P Percentage
C 69.  Manweopper          REAL             %                  Manure Water Extractable organic P Percentage
C 70.  Manwcper            REAL             %                  Manure Dry matter percentage
C 71.  Manfc               REAL             %                  Manure Field Covered percentage
C 72.  Mand                REAL             m                  Manure Depth of Application
C 73.  Mlefts              REAL             %                  Percentage of Manure left of surface durringe application
C 74.  Slnm                INTEGER          -                  Serial Number of the Manure applied
C 75.  Mtype               INTEGER          -                  Manure Type Applied
C 76.  Mancova             REAL             ha                 Manure Coverage Area
C 77.  Manmoist            REAL             Kg/Kg              Manure Moisture Content
C 78.  Appdaymanmass       REAL             Kg                 Application day Manure Mass
C 79.  Mandcom             REAL             Kg                 Manure Decomposition
C 80.  Mancovadcom         REAL             ha                 Manure Coverage area decomposition
C 81.  Mansipdcom          REAL             Kg                 Manure stable inorganic P decomposition
C 82.  Mansopdcom          REAL             Kg                 Manure stable organic P decomposition
C 83.  Manwopdcom          REAL             Kg                 Manure water extractable organic P decomposition
C 84.  Manasim             REAL             Kg                 Manure Assimilation
C 85.  Mancovaasim         REAL             ha                 Manure Coverarea Assismilation
C 86.  Manwipasim          REAL             Kg                 Manure water extractable inorganic P Assismilation
C 87.  Manwopasim          REAL             Kg                 Manure water extractable organic P Assismilation
C 88.  Mansipasim          REAL             Kg                 Manure Stable inorganic P assimilation
C 89.  Mansopasim          REAL             Kg                 Manure Stable organic P assimilation
C 90.  Maniprelease        REAL             Kg                 Daily Manure inorganic P Release due to rainfall
C 91.  Manoprelease        REAL             Kg                 Daily Manure organic P Release due to rainfall
C.92.  Manipcrunoff        REAL             Kg/m3              Manure inorganic P Concentration in Runoff
C 93.  Manopcrunoff        REAL             Kg/m3              Manure organic P Concentration in Runoff
C 94.  Manipmrunoff        REAL             Kg                 Manure inorganic P mass in Runoff
C 95.  Manopmrunoff        REAL             Kg                 Manure organic P mass in Runoff
C 96.  Mandcomr            REAL             per day            Manure decomposition rate
C 97.  Manasimr            REAL             per day            Manure Assimilation rate
C 98.  Manextrc            REAL             -                  Manure Exraction Coefficient
C 99.  Ntill               INTEGER          -                  Total Number of tillage operation during the simulation period       
C 100. Tday                INTEGER          -                  Tillage application day
C 101. Tmon                INTEGER          -                  Tilaage application month
C 102. Tyear               INTEGER          -                  Tillage application year
C 103. Tillinceffi         REAL             %                  Tillage incorporation efficiency
C 104. Tillmixeffi         REAL             %                  Tillage mixing efficiency
C 105. Tilld               REAL             m                  Tillage application depth
C 106. Slnt                INTEGER          -                  Serial No of Tillage beling applied
C 107. Ncrop               INTEGER          -                  Total number of crop durring the simulation period
C 108. Cropname            String           -                  Name of the Crop
C 109. Cpday               INTEGER          -                  Crop Planting date
C 110. Cpmon               INTEGER          -                  Crop Planting month
C 111. Cpyear              INTEGER          -                  Crop Planting year
C 112. Chday               INTEGER          -                  Crop harvest day
C 113. Chmon               INTEGER          -                  Crop harvest month
C 114. Chyear              INTEGER          -                  Crop harvest year
C 115. Biopfracemg         REAL             -                  Biomass P fraction at crop emergence
C 116. Biopfracmat         REAL             -                  Biomass P fraction at crop maturity
C 117. Basetemp            REAL             -                  Base Temerature
C 118. Pupdist             REAL             -                  P uptake distribution parameter
C 119. Cropday             BOOLEAN          -                  Flag to check if on a particular day there is
C                                                              crop in the field or not
C 120. Phumat              REAL             -                  Potential Heat Unit at crop maturity 
C 121. Slnc                INTEGER          -                  Serial No if the Crop is being simulated
C 122. Hu                  REAL             -                  Daily Crop Growing Heat Unit
C 123. Biopfrac            REAL             -                  Biomass P fraction of a crop on a day
C 124. Actbiomass          REAL             -                  Actual Crop Biomass on a day
C 125. Potbiomass          REAL             -                  Potential Biomass on a day
C 126. Rootd               REAL             -                  Root depth on a day
C 127. Biopopt             REAL             Kg/ha              Optimum Biomass P in a day
C 128. Biopact             REAL             Kg/ha              Actual Biomass P in a day
C 129. Dlypotbiomsincrs    REAL             Kg/ha              Daily Potential Biomass increase
C 130. Plantpdemand        REAL             Kg/ha              Daily Plant P Demand
C 131. Potpuptake          REAL             Kg/ha              Potential P Uptake of a soil layer
C 132. Actpuptake          REAL             Kg/ha              Actual P Uptake of a soil layer
C 133. Daydesrp            INTEGER          Day                Days since P desorption From Active P Pool to Labile P Pool started
C 134. Daysrp              INTEGER          Day                Days since P sorption From Labile P Pool to Active P Pool started
C 135. Pflowlabact         REAL             Kg/ha              P Flux From Labile P Pool to Active P Pool
C 136. Pflowactlab         REAL             Kg/ha              P Flux From Active P Pool to Labile P Pool
C 137. Pflowactstbi        REAL             Kg/ha              P Flux From Active P Pool to Stable Inorganic Pool
C 138. Pflowstbiact        REAL             Kg/ha              P Flux From Stable Inorganic Pool to Active P Pool
C 139. Soiltemp            REAL             0C                 Soil Temperature
C 140. Soilwater           REAL             m3/m3              Soil Water
C 141. Soilnfresh          REAL             Kg/ha              Soil Fresh N Pool
C 142. Soilno3             REAL             Kg/ha              Soil Solution N Pool
C 143. Resdcofi            REAL             Day-1              Residue Decompostion Coefficient
C 144. Inicrpres           REAL             Kg/Ha              Initial Crop Residue
C 145. Pflowresfrso        REAL             Kg/ha              P Flux from Crop Residue to Fresh Organic P Pool
C 146. Pflowfrsolab        REAL             Kg/ha              P Flux from Fresh Organic P Pool to Labile P Pool
C 147. Pflowfrsostbo       REAL             Kg/ha              P Flux from Fresh Organic P Pool to Stable Organic P Pool
C 148. Pflowlabfrso        REAL             Kg/ha              P Flux from Labile P Pool to Fresh Organic P Pool
C 149. Crpresleft          REAL             Kg/ha              Crop Residue left a field at the harvest day   
C 150. Labpmlossrnf        REAL             Kg/ha              Soil P mass which is lost from Labile P pool through runoff
C 151. Manpmlossrnf        REAL             Kg/ha              Manure P mass Loss through runoff
C 152. Fertpmlossrnf       REAL             Kg/ha              Fertilizer P mass loss through runoff
C 153. Drplossrnf          REAL             Kg/ha              Disolved Reactive P loss through runoff
C 154. PPcactp             REAL             g/MT               PP Concentration in soil due to Active P Pool
C 155. PPcstabip           REAL             g/MT               PP Concentration in soil due Stabile Inorganic P Pool
C 156. PPcstabop           REAL             g/MT               PP Concentration in soil due Stabile Organic P Pool 
C 157. PPcfrsop            REAL             g/MT               PP Concentration in soil due Fresh Organic P Pool 
C 158. Sed                 REAL             MetricTon/Day      Sediment yeild from the field
C 159. Peakrunoff          REAL             m3/sec             Peak rate of Runoff
C 160. Pusle               REAL             -                  USLE Support practice factor
C 161. Cmusle              REAL             -                  USLE Crop and Management factor
C 162. Lslp                REAL             -                  Length of the Slope
C 163. Slpper              REAL             -                  Slope Percentage
C 164. Slpangl             REAL             Degree             Slope Angle
C 165. Rock                REAL             %                  Rock percentage on the soil surface
C 166. PPmactp             REAL             Kg/ha              PP mass loss from Active P Pool
C 167. PPmstabip           REAL             Kg/ha              PP mass loss from Stabile Inorganic P Pool
C 168. PPcstabop           REAL             Kg/ha              PP mass loss from Stabile Organic P Pool
C 169. PPcfrsop            REAL             Kg/ha              PP mass loss from Fresh Organic P Pool 
C 170. PPlossrnf           REAL             Kg/ha              Particulate P loss through runoff
C 171. TotalsoilP          REAL             Kg/ha              Total Soil P on a day
C 172. Avgrbiom            REAL             Kg/Ha              Above Ground Biomass
C 173. Blgrbiom            REAL             Kg/ha              Below Ground Biomass
C 174. Avgrres             REAL             Kg/ha              Above Ground Crop Residue
C 175. Yeild               REAL             Kg/ha              Yeild of Crop
C 176. Nirri               REAL             -                  Number of Irrigation applied
C 177. Irri                REAL             Cm                 Amount of Irrigation applied
C 178. Temp                REAL             Degree C           Average Atmospheric Temperature
C 179. Rain                REAL             Cm                 Rainfall amount
C 180. Runoff              REAL             Cm                 Runoff amount
C 181. Appeffi             REAL             %                  Irrigation Application Efficiency
C 182. Iday                INTEGER          -                  Irrigation application day
C 183. Imon                INTEGER          -                  Irrigation application month
C 184. Iyear               INTEGER          -                  Irrigation application year
C 185. Clabpsw             REAL             Kg/m3              Concentration of Labile P in soil water
C 186. Massly              REAL             Kg/m2              Mass of a soil layer
C 187. Kd                  REAL             m3/Kg              Soil Partitioing Coefficient
C 188. Matflow             REAL             cm                 Matrix flow i.e. percolating water out of a soil layer
C 189. Dimvly              REAL             m                  Depth to impervious soil layer from ground level
C 190. Dgwt                REAL             m                  Depth to ground water table from ground level
C 191. Pleachmat           REAL             Kg/m3              P leaching through matrix flow
C 192. Pleachmac           REAL             Kg/m3              P leaching through macropore flow
C 193. Macflow             REAL             m                  Macropore flow
C 194. Cdrpgw              REAL             Kg/m3              Concentration of DRP in ground water reservoir
C 195. Hgwt                REAL             m                  Hight if Ground water table over impervious layer
C 196. Mdrpgw              REAL             Kg                 Mass of DRP in ground water
C 197. Tiledrain           REAL             m                  Daily Tile drainage amount
C 198. Drplosstdrain       REAL             Kg/ha              DRP loss throgh tile drainage
C 199. Inidrpmgw           REAL             Kg                 Initial DRP mass in groundwater reservoir
C 200. PPlosstdrain        REAL             Kg/ha              PP loss through tile draing
C 201. Kr                  REAL             gm/(m2-day)        Soil replenishment rate coefficient
C 202. Kdh                 REAL             gm/J               Soil Detachibility coefficient
C 203. Kf                  REAL             /m                 Soil Filter coefficient
C 204. LAI                 REAL             -                  Leaf Area Index
C 205. Plnth               REAL             m                  Plant Height
C 206. Plntr               RAL              m                  Plant Row spacing
C 207. Ms                  REAL             gm/gm soil         Mass of readily available dispersible soil particle
C 208. Dtile               REAL             m                  Depth to tile drain.
C 209. PPlabp              REAL             Kg/ha              PP loss from Labile P Pool
C 210. PPactp              REAL             Kg/ha              PP loss from active P pool
C 211. PPstabip            REAL             Kg/ha              PP loss from stable inorganic P pool
C 212. PPfrsop             REAL             Kg/ha              PP loss from fresh organic P pool
C 213. PPstabop            REAL             Kg/ha              PP loss from stable organic P Pool
C 214. PPmanwip            REAL             Kg/ha              PP loss from manure water extractable inorganic P Pool
C 215. PPmanwop            REAL             Kg/ha              PP loss from manure water extractable organic P Pool
C 216. PPmansop            REAL             Kg/ha              PP loss from manure stable organic P Pool
C 217. PPmansip            REAL             Kg/ha              PP loss from manure stable inorganic P Pool
C 218. PPavfertp           REAL             Kg/ha              PP loss from available fertilizer P pool
C 219. PPresfertp
C 220. PPmanp              REAL             Kg/ha              PP loss from manure P Pool.
C 221. PPfertp             REAL             Kg/ha              PP loss from fertilizer P pool
C 222. Mppgw               REAL             Kg                 Mass of PP in groundwater reservoir
C 223. Inippmgw            REAL             Kg                 Initial mass of PP in groundwater reservoir
C 224. Cppgw               REAL             Kg/m3              Concentration of PP in grundwater reservoir
C 225. PPclabp             REAL             g/MT               PP Concentration in soil due to Lab P Pool
C 226. PPmlabp             REAL             Kg/ha              PP loss from labile P pool 
C 227. PPcmanwip           REAL             g/MT               PP Concentration in soil due to MANWIP P Pool
C 228. PPcmanwop           REAL             g/MT               PP Concentration in soil due to MANWOP P Pool
C 229. PPcmansip           REAL             g/MT               PP Concentration in soil due to MANSIP P Pool
C 230. PPcmansop           REAL             g/MT               PP Concentration in soil due to MANSOP P Pool
C 231. PPmmanwip           REAL             Kg/ha              PP mass loss from  MANWIP P Pool
C 232. PPmmanwop           REAL             Kg/ha              PP mass loss from  MANWOP P Pool
C 233. PPmmansip           REAL             Kg/ha              PP mass loss from  MANSIP P Pool
C 234. PPmmansop           REAL             Kg/ha              PP mass loss from  MANSOP P Pool
C 235. PPmmanp             REAL             Kg/ha              PP mass loss from  ALL MANURE P Pool
C 236. PPcavfertp          REAL             g/MT               PP Concentration in soil due to Available fertilizer P Pool
C 237. PPcresfertp         REAL             g/MT               PP Concentration in soil due to residual fertilizer P Pool
C 238. PPmavfertp          REAL             Kg/ha              PP mass loss from Available fertilizer P Pool
C 239. PPmresfertp         REAL             Kg/ha              PP mass loss from residual fertilizer P Pool
C 240. PPmfertp            REAL             Kg/ha              PP mass loss from all fertilizer P Pool
C 250. Sedi                REAL             MT/day             Sediment Yeild from the Field
C 251.Mint                REAL             Degree C           Minumum Temperature
C 252. Maxt                REAL             Degree C           Maximum Temperature
C 253. Flag                LOGICAL          -                  A LOGICAL VARIABLE
C 254. Tdparti             REAL             gm/m2/day          Total mass of detached of soil particle
C 255. Dnode               REAL             m                  Depth of Soil Node
C 256. Nnode               INTEGER          -                  Number of Nodes
C 257. Tnode               REAL             m                  Thickness of a soil node
C 258. Labpnode            REAL             Kg/ha              Labile P of each soil node   
C 259. Claynode            REAL             %                  Clay percentage of a soil node  
C 260. BDsoilnode          REAL             Kg/m3              Bulkdensity of Soil node
C 261. Soilwaternode       REAL             m3/m3              Soil water at each soil node
C 262. Pextr               REAL             -                  P extraction Coefficient
C  Added by Zhiming Qi 
C 263. RatioSIPtoAIP       REAL             -                   Stable Inorganic P:Active Inorganic P


C====================================================================================================================
C                         END OF SPECIFICATION
C===============================================================================
       INTEGER :: Syear, Eyear,Sday,Eday,Smon,Emon,Nsoil,Slnf,Nirri
       
       INTEGER :: Nfert,Nman,Slnm,Ntill,Slnt,Ncrop,Slnc,
     +            Nnode,Counternode1,Countman,PLN,PLF,Countfert,PLT,
     +  Counttill,PrevYear
    
       
       INTEGER,DIMENSION(:),ALLOCATABLE :: Fday,Fyear,Fmon,Scov,Norain,
     + Mday1,Mmon,Myear1,Mtype,Tday1,Tmon,Tyear,Cpday,Cpmon,Cpyear,Chday
     + ,Chmon,Chyear,Daydesrp,Daysrp,Iday1,Imon,Iyear1,mmm,Mtype1,Pstrs,
     + PlntPstress 
       
      DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: Bdsoil,Pdsoil,Ksat,
     + Fc,Pwp, 
     + Psc,Clayp,Sandp,Siltp,Orgm, Labp,Actp,Stabip,Stabop,Frsop,Crpres,
     + Tsoil,Dsoil,Orgc, Smc,Fertp,Fertd,Flefts,Avfertp,
     + Daysfert,Resfertp,Fertpfr,Fertpsorp,Appdayavfertp,
     + Appdayresfertp,Dummy2,Dailyfertpsop,Fertprelease,Fertpcrunoff,
     + Fertpmrunoff,Manmass,Manpmass,Mantpper,Manweipper,Manweopper,
     + Manwcper,Manfc,Mand,Mlefts,Manwip,Manwop,Mansop,Mansip,Mancova,
     + Manmoist,Appdaymanmass,Mandcom,Mancovadcom,Mansipdcom,Mansopdcom,
     + Manwopdcom,Manasim,Mancovaasim,Manwipasim,Manwopasim,Mansipasim,
     + Mansopasim,Maniprelease,Manoprelease,Inicrpres,Pflowresfrso,
     + Pflowfrsolab, Pflowfrsostbo,Pflowlabfrso,Pflowstbolab,Clabpsw,
     + Massly,Kd,Pleachmat,Plntr,PPmanwip,PPmanwop,PPmansop,
     + PPmansip,PPavfertp,PPresfertp,PPcmanwop,PPcmanwip,PPcmansip,
     + PPcmansop,PPmmanwip,PPmmanwop,PPmmansip,PPmmansop,Labpnode,
     + Claynode,BDsoilnode,Actpnode,Stabipnode,Stabopnode,Frsopnode,
     + ResdueP1,ResdueP2,HumusP1,HumusP2,HumusP3,Pflowr1h2,
     + Pflowr1h1,Pflowr2h1,Pflowh1h2,Pflowh2h1,Pflowh2h3,Pflowh3h1,
     + Pflowr1frsop,Pflowr2frsop,Pflowh1stbop,Pflowh2stbop,Pflowh3stbop,
     + ResdueC1,ResdueC2,HumusC1,HumusC2,HumusC3,SOroot,CProot,
     + Blgrbiomly,BlgrPly,ResdueP1node,ResdueP2node,HumusP1node,
     + HumusP2node,HumusP3node,Pflowr1frsopnode,Pflowr2frsopnode,
     + Pflowh1stbopnode,Pflowh2stbopnode,Pflowh3stbopnode,AppdayManmass1
     + ,Mantpper1,Manweipper1,Manweopper1,Manwcper1,Manfc1,Mand1,
     + Mlefts1,FertP1,Fertd1,Flefts1,Scov1,RatioSIPtoAIP 
            
      
       DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: Manipcrunoff,
     +  Manopcrunoff,Pstress,
     + Manipmrunoff,Manopmrunoff,Mandcomr,Manasimr,Manextrc,Tillinceffi,
     + Tillmixeffi,Tilld,Biopfracemg,Biopfracmat,Basetemp,Pupdist,Phumat
     + ,Hu,Biopfrac,Biopfrac50mat,Biopopt,Biopact,Plantpdemand,
     + Potpuptake,Actpuptake,Pflowlabact,Pflowactlab,Pflowactstbi,
     + Pflowstbiact,Resdcofi,Crpresleft,Avgrres,Irri,Appeffi,PPmavfertp,
     + PPcavfertp, PPcresfertp,PPmresfertp,Dnode,Tnode,m12flow,
     + Tillinceffi1,Tillmixeffi1,Tilld1,FertPLabP1,FertPLabP2,
     + ManSO_WI,ManSI_WI,ManWO_WI,ManSO_WO,ManWI_LabP1,ManWI_LabP2,
     + ManWI_LabP3,ManWO_LabP1,ManWO_LabP2,ManWO_LabP3,ManSO_LabP1,
     + ManSO_LabP2,ManSO_LabP3,ManSI_ActP1,Soilno3,Matflow,Soilwaternode
     + ,ManSI_ActP2,ManSI_ActP3,Soiltemp,Soilwater,Soilnfresh
       
       DOUBLE PRECISION,DIMENSION(5) :: Pdcomr,CPini
       
       DOUBLE PRECISION, DIMENSION(:,:) , ALLOCATABLE :: CP,Pdrn
       
       
       CHARACTER(LEN=255):: Scnraioname
       CHARACTER(LEN=10),DIMENSION(:),ALLOCATABLE :: Cropname
       
      DOUBLE PRECISION :: Areap,Labpmlossrnf,Manpmlossrnf,Fertpmlossrnf,
     + Drplossrnf,Mgwp,Pflowresfrso1,
     + PPcactp,PPcstabip,PPcstabop,PPcfrsop,Sed,Pusle,Cmusle,Lslp,
     + Slpper,Slpangl,Rock,PPmactp,PPmstabip,PPmstabop,PPmfrsop,
     + PPlossrnf,TotalsoilP,Dimvly,Pleachmac,Cdrpgw,Hgwt,Mdrpgw,
     + Drplosstdrain,Inidrpmgw,PPlosstdrain,Kdh,Kr,Kf,Ms,Dtile,PPlabp,
     + PPactp,PPstabip,PPfrsop,PPstabop,PPmanp,PPfertp,Mppgw,Inippmgw,
     + Cppgw,PPclabp,PPmlabp,PPmmanp,PPmfertp,Tdparti,Pextr,Dummy1,
     + m11flow,Coefi14,Coefi23,Coefi25,Coefi34,ResdueP0,CP0,
     + Coefi45,Coefi15,ResdueC0,ADDPLANTP,Totm,Totfrt,Drplossdesp,
     + Drplosslatflow,PPlossdesp,PPlosslatflow,TotalResHumP,Tempu1,
     + Tempu2,Pst,Inidrpmacgw,Mdrpmacgw,Cdrpmacgw,Drpmactdrain,TotManwip
     + ,TotManwop,TotMansip,TotMansop,TotAvfertp,TotResfertp,
     + TotFertPLabP1,TotFertPLabP2,TotManWI_LabP1,TotManWI_LabP2,
     + TotManWI_LabP3,TotManWO_LabP1,TotManWO_LabP2,TotManWO_LabP3,
     + TotManSO_LabP1,TotManSO_LabP2,TotManSO_LabP3,TotManSI_ActP1,     
     + TotManSI_ActP2,TotManSI_ActP3,TotManSO_WI,TotManSI_WI,TotManWO_WI
     + ,TotManSO_WO,IniSoilP,FinalSoilP,FertilizerP,ManuP,ResP,PlntP_Up,
     + Rain,Tempt,Mint,Maxt,Runoff,Actbiomass,Latflow,Deepsepg,
     + Potavgbiomass,Potblgbiomass,Potyeild,Potbiomass,Rootd,
     + Dlypotbiomsincrs,Peakrunoff,Avgrbiom,Blgrbiom,Yeild,Dgwt,Macflow,
     + Tiledrain,LAIP,Plnth,Sedi,PlntP
      
       
      
       
       LOGICAL :: Cropday,Flag,Psts
      
       END MODULE VARIABLE
      