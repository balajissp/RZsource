#!/bin/bash
FORCE=false

# https://stackoverflow.com/questions/2953646/how-can-i-declare-and-use-boolean-variables-in-a-shell-script/21210966#21210966
while test $# -gt 0; do
  case "$1" in
    -f|--force)
      echo -e "using force flag, all files will be compiled from source\n"
      FORCE=true
	  break
	  ;;
    *)
	  echo -e "compiling only files that have changed.\n"
      break
      ;;
  esac
done

if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_WBCAN.FOR -nt ./Release/SHAW_WBCAN.o ]
then
    echo -e "compiling SHAW_WBCAN"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_WBCAN.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Photosyn.for -nt ./Release/SHAW_Photosyn.o ]
then
    echo -e "compiling SHAW_Photosyn"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Photosyn.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRBAL.FOR -nt ./Release/SHAW_LWRBAL.o ]
then
    echo -e "compiling SHAW_LWRBAL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRBAL.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_PHENOL.FOR -nt ./Release/SG_PHENOL.o ]
then
    echo -e "compiling SG_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_PHENOL.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzpet.for -nt ./Release/Rzpet.o ]
then
    echo -e "compiling Rzpet"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzpet.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/DSSATNFIX.FOR -nt ./Release/DSSATNFIX.o ]
then
    echo -e "compiling DSSATNFIX"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/DSSATNFIX.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_VSLOPE.FOR -nt ./Release/SHAW_VSLOPE.o ]
then
    echo -e "compiling SHAW_VSLOPE"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_VSLOPE.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_TRANSC.FOR -nt ./Release/SHAW_TRANSC.o ]
then
    echo -e "compiling SHAW_TRANSC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_TRANSC.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESVK.FOR -nt ./Release/SHAW_RESVK.o ]
then
    echo -e "compiling SHAW_RESVK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESVK.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_META.FOR -nt ./Release/SHAW_META.o ]
then
    echo -e "compiling SHAW_META"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_META.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_CANTK.For -nt ./Release/SHAW_CANTK.o ]
then
    echo -e "compiling SHAW_CANTK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_CANTK.For
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_NFACTO.for -nt ./Release/PT_NFACTO.o ]
then
    echo -e "compiling PT_NFACTO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_NFACTO.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/HERMES_Radia.for -nt ./Release/HERMES_Radia.o ]
then
    echo -e "compiling HERMES_Radia"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/HERMES_Radia.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Weight.for -nt ./Release/SHAW_Weight.o ]
then
    echo -e "compiling SHAW_Weight"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Weight.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SOILR.FOR -nt ./Release/SHAW_SOILR.o ]
then
    echo -e "compiling SHAW_SOILR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SOILR.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SNOWHT.FOR -nt ./Release/SHAW_SNOWHT.o ]
then
    echo -e "compiling SHAW_SNOWHT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SNOWHT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LEAFT.for -nt ./Release/SHAW_LEAFT.o ]
then
    echo -e "compiling SHAW_LEAFT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LEAFT.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Conduc.for -nt ./Release/SHAW_Conduc.o ]
then
    echo -e "compiling SHAW_Conduc"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Conduc.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzchem.for -nt ./Release/Rzchem.o ]
then
    echo -e "compiling Rzchem"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzchem.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/KghaToKg.for -nt ./Release/KghaToKg.o ]
then
    echo -e "compiling KghaToKg"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/KghaToKg.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/FREEZE.FOR -nt ./Release/FREEZE.o ]
then
    echo -e "compiling FREEZE"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/FREEZE.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Comp2exp.for -nt ./Release/Comp2exp.o ]
then
    echo -e "compiling Comp2exp"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Comp2exp.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Weather/SOLAR.FOR -nt ./Release/SOLAR.o ]
then
    echo -e "compiling SOLAR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Weather/SOLAR.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SWRBAL.FOR -nt ./Release/SHAW_SWRBAL.o ]
then
    echo -e "compiling SHAW_SWRBAL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SWRBAL.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_FSLOPE.FOR -nt ./Release/SHAW_FSLOPE.o ]
then
    echo -e "compiling SHAW_FSLOPE"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_FSLOPE.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Frozen.for -nt ./Release/SHAW_Frozen.o ]
then
    echo -e "compiling SHAW_Frozen"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Frozen.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_CANHUM.FOR -nt ./Release/SHAW_CANHUM.o ]
then
    echo -e "compiling SHAW_CANHUM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_CANHUM.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Qckturf.for -nt ./Release/Qckturf.o ]
then
    echo -e "compiling Qckturf"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Qckturf.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/HERMES_SetupCrop.for -nt ./Release/HERMES_SetupCrop.o ]
then
    echo -e "compiling HERMES_SetupCrop"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/HERMES_SetupCrop.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/CSUTS040.FOR -nt ./Release/CSUTS040.o ]
then
    echo -e "compiling CSUTS040"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/CSUTS040.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_TRANSR.FOR -nt ./Release/SHAW_TRANSR.o ]
then
    echo -e "compiling SHAW_TRANSR"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_TRANSR.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRMAT.FOR -nt ./Release/SHAW_LWRMAT.o ]
then
    echo -e "compiling SHAW_LWRMAT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRMAT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRATM.FOR -nt ./Release/SHAW_LWRATM.o ]
then
    echo -e "compiling SHAW_LWRATM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRATM.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Canlay.for -nt ./Release/SHAW_Canlay.o ]
then
    echo -e "compiling SHAW_Canlay"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Canlay.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Solvrad.for -nt ./Release/SHAW_Solvrad.o ]
then
    echo -e "compiling SHAW_Solvrad"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Solvrad.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SNOWBC.FOR -nt ./Release/SHAW_SNOWBC.o ]
then
    echo -e "compiling SHAW_SNOWBC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SNOWBC.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_NWSNOW.FOR -nt ./Release/SHAW_NWSNOW.o ]
then
    echo -e "compiling SHAW_NWSNOW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_NWSNOW.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_EBCAN.FOR -nt ./Release/SHAW_EBCAN.o ]
then
    echo -e "compiling SHAW_EBCAN"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_EBCAN.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Qckplnt.for -nt ./Release/Qckplnt.o ]
then
    echo -e "compiling Qckplnt"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Qckplnt.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_PHASEI.FOR -nt ./Release/ML_PHASEI.o ]
then
    echo -e "compiling ML_PHASEI"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_PHASEI.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SWRSNO.FOR -nt ./Release/SHAW_SWRSNO.o ]
then
    echo -e "compiling SHAW_SWRSNO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SWRSNO.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_STMMLT.FOR -nt ./Release/SHAW_STMMLT.o ]
then
    echo -e "compiling SHAW_STMMLT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_STMMLT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESGMC.FOR -nt ./Release/SHAW_RESGMC.o ]
then
    echo -e "compiling SHAW_RESGMC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESGMC.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_GOSHAW.FOR -nt ./Release/SHAW_GOSHAW.o ]
then
    echo -e "compiling SHAW_GOSHAW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_GOSHAW.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_ADJUST.FOR -nt ./Release/SHAW_ADJUST.o ]
then
    echo -e "compiling SHAW_ADJUST"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_ADJUST.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/LINDM.FOR -nt ./Release/LINDM.o ]
then
    echo -e "compiling LINDM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/LINDM.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Horizon.for -nt ./Release/Horizon.o ]
then
    echo -e "compiling Horizon"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Horizon.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/CSREA040.FOR -nt ./Release/CSREA040.o ]
then
    echo -e "compiling CSREA040"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/CSREA040.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SUMDT.FOR -nt ./Release/SHAW_SUMDT.o ]
then
    echo -e "compiling SHAW_SUMDT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SUMDT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SNOALB.FOR -nt ./Release/SHAW_SNOALB.o ]
then
    echo -e "compiling SHAW_SNOALB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SNOALB.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESHT.FOR -nt ./Release/SHAW_RESHT.o ]
then
    echo -e "compiling SHAW_RESHT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESHT.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_ROOTGR.for -nt ./Release/SG_ROOTGR.o ]
then
    echo -e "compiling SG_ROOTGR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_ROOTGR.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_NFACT.FOR -nt ./Release/SG_NFACT.o ]
then
    echo -e "compiling SG_NFACT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_NFACT.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_rootgr.for -nt ./Release/ML_rootgr.o ]
then
    echo -e "compiling ML_rootgr"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_rootgr.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Solar.for -nt ./Release/SHAW_Solar.o ]
then
    echo -e "compiling SHAW_Solar"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Solar.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESTK.FOR -nt ./Release/SHAW_RESTK.o ]
then
    echo -e "compiling SHAW_RESTK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESTK.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_EBSNOW.FOR -nt ./Release/SHAW_EBSNOW.o ]
then
    echo -e "compiling SHAW_EBSNOW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_EBSNOW.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzplnt.for -nt ./Release/Rzplnt.o ]
then
    echo -e "compiling Rzplnt"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzplnt.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_STAB.FOR -nt ./Release/SHAW_STAB.o ]
then
    echo -e "compiling SHAW_STAB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_STAB.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_QVSOIL.FOR -nt ./Release/SHAW_QVSOIL.o ]
then
    echo -e "compiling SHAW_QVSOIL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_QVSOIL.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_EBSOIL.FOR -nt ./Release/SHAW_EBSOIL.o ]
then
    echo -e "compiling SHAW_EBSOIL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_EBSOIL.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Cloudy.for -nt ./Release/SHAW_Cloudy.o ]
then
    echo -e "compiling SHAW_Cloudy"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Cloudy.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_lindm.for -nt ./Release/for_lindm.o ]
then
    echo -e "compiling for_lindm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_lindm.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_WBALNC.FOR -nt ./Release/SHAW_WBALNC.o ]
then
    echo -e "compiling SHAW_WBALNC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_WBALNC.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_source.for -nt ./Release/SHAW_source.o ]
then
    echo -e "compiling SHAW_source"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_source.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SOILHT.FOR -nt ./Release/SHAW_SOILHT.o ]
then
    echo -e "compiling SHAW_SOILHT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SOILHT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRRES.FOR -nt ./Release/SHAW_LWRRES.o ]
then
    echo -e "compiling SHAW_LWRRES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRRES.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzpest.for -nt ./Release/Rzpest.o ]
then
    echo -e "compiling Rzpest"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzpest.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Qcktree.for -nt ./Release/Qcktree.o ]
then
    echo -e "compiling Qcktree"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Qcktree.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_PHENOL.FOR -nt ./Release/ML_PHENOL.o ]
then
    echo -e "compiling ML_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_PHENOL.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_sdcomp.for -nt ./Release/for_sdcomp.o ]
then
    echo -e "compiling for_sdcomp"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_sdcomp.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_WBSNOW.FOR -nt ./Release/SHAW_WBSNOW.o ]
then
    echo -e "compiling SHAW_WBSNOW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_WBSNOW.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SWRCAN.FOR -nt ./Release/SHAW_SWRCAN.o ]
then
    echo -e "compiling SHAW_SWRCAN"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SWRCAN.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SOILTK.FOR -nt ./Release/SHAW_SOILTK.o ]
then
    echo -e "compiling SHAW_SOILTK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SOILTK.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESVAP.FOR -nt ./Release/SHAW_RESVAP.o ]
then
    echo -e "compiling SHAW_RESVAP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESVAP.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_TDMA.FOR -nt ./Release/SHAW_TDMA.o ]
then
    echo -e "compiling SHAW_TDMA"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_TDMA.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_QVSNOW.FOR -nt ./Release/SHAW_QVSNOW.o ]
then
    echo -e "compiling SHAW_QVSNOW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_QVSNOW.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRSNO.FOR -nt ./Release/SHAW_LWRSNO.o ]
then
    echo -e "compiling SHAW_LWRSNO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRSNO.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_BACKUP.FOR -nt ./Release/SHAW_BACKUP.o ]
then
    echo -e "compiling SHAW_BACKUP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_BACKUP.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_PHASEI.FOR -nt ./Release/SG_PHASEI.o ]
then
    echo -e "compiling SG_PHASEI"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_PHASEI.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/KgtoKgha.for -nt ./Release/KgtoKgha.o ]
then
    echo -e "compiling KgtoKgha"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/KgtoKgha.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SNOMLT.FOR -nt ./Release/SHAW_SNOMLT.o ]
then
    echo -e "compiling SHAW_SNOMLT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SNOMLT.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_QVRES.FOR -nt ./Release/SHAW_QVRES.o ]
then
    echo -e "compiling SHAW_QVRES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_QVRES.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzrich.for -nt ./Release/Rzrich.o ]
then
    echo -e "compiling Rzrich"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzrich.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzout.for -nt ./Release/Rzout.o ]
then
    echo -e "compiling Rzout"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzout.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/readrzx.for -nt ./Release/readrzx.o ]
then
    echo -e "compiling readrzx"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/readrzx.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Nodethick.for -nt ./Release/Nodethick.o ]
then
    echo -e "compiling Nodethick"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Nodethick.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/IPPARM.for -nt ./Release/IPPARM.o ]
then
    echo -e "compiling IPPARM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/IPPARM.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_WBRES.FOR -nt ./Release/SHAW_WBRES.o ]
then
    echo -e "compiling SHAW_WBRES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_WBRES.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RESHUM.FOR -nt ./Release/SHAW_RESHUM.o ]
then
    echo -e "compiling SHAW_RESHUM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RESHUM.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_FROST.FOR -nt ./Release/SHAW_FROST.o ]
then
    echo -e "compiling SHAW_FROST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_FROST.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/SDCOMP.FOR -nt ./Release/SDCOMP.o ]
then
    echo -e "compiling SDCOMP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/SDCOMP.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/RZTEST.for -nt ./Release/RZTEST.o ]
then
    echo -e "compiling RZTEST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/RZTEST.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/REF_ET.FOR -nt ./Release/REF_ET.o ]
then
    echo -e "compiling REF_ET"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/REF_ET.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ipparm.for -nt ./Release/for_ipparm.o ]
then
    echo -e "compiling for_ipparm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ipparm.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_Update.for -nt ./Release/SHAW_Update.o ]
then
    echo -e "compiling SHAW_Update"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_Update.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_RTDIST.for -nt ./Release/SHAW_RTDIST.o ]
then
    echo -e "compiling SHAW_RTDIST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_RTDIST.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_ENHANC.FOR -nt ./Release/SHAW_ENHANC.o ]
then
    echo -e "compiling SHAW_ENHANC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_ENHANC.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_ATstab.for -nt ./Release/SHAW_ATstab.o ]
then
    echo -e "compiling SHAW_ATstab"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_ATstab.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/RZTEMP.FOR -nt ./Release/RZTEMP.o ]
then
    echo -e "compiling RZTEMP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/RZTEMP.FOR
fi
if [ "$FORCE" = true ] || [ ./GLEAMS/RZ-Erosion.for -nt ./Release/RZ-Erosion.o ]
then
    echo -e "compiling RZ-Erosion"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./GLEAMS/RZ-Erosion.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_PHASEI.for -nt ./Release/PT_PHASEI.o ]
then
    echo -e "compiling PT_PHASEI"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_PHASEI.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_NFACT.FOR -nt ./Release/ML_NFACT.o ]
then
    echo -e "compiling ML_NFACT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_NFACT.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Countdata.for -nt ./Release/Countdata.o ]
then
    echo -e "compiling Countdata"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Countdata.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Snowprms.for -nt ./Release/Snowprms.o ]
then
    echo -e "compiling Snowprms"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Snowprms.for
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_SNOWTK.FOR -nt ./Release/SHAW_SNOWTK.o ]
then
    echo -e "compiling SHAW_SNOWTK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_SNOWTK.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_LWRCAN.FOR -nt ./Release/SHAW_LWRCAN.o ]
then
    echo -e "compiling SHAW_LWRCAN"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_LWRCAN.FOR
fi
if [ "$FORCE" = true ] || [ ./SHAW24b/SHAW_EBRES.FOR -nt ./Release/SHAW_EBRES.o ]
then
    echo -e "compiling SHAW_EBRES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./SHAW24b/SHAW_EBRES.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/NO_N2O_RATIO.FOR -nt ./Release/NO_N2O_RATIO.o ]
then
    echo -e "compiling NO_N2O_RATIO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/NO_N2O_RATIO.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Input/LMATCH.FOR -nt ./Release/LMATCH.o ]
then
    echo -e "compiling LMATCH"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Input/LMATCH.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/HERMES_SUCROS.for -nt ./Release/HERMES_SUCROS.o ]
then
    echo -e "compiling HERMES_SUCROS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/HERMES_SUCROS.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/ModuleDefs.for -nt ./Release/ModuleDefs.o ]
then
    echo -e "compiling ModuleDefs"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/ModuleDefs.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Variable.for -nt ./Release/Variable.o ]
then
    echo -e "compiling Variable"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Variable.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/CsvOuts/csvlinklist.f90 -nt ./Release/csvlinklist.o ]
then
    echo -e "compiling csvlinklist"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/CsvOuts/csvlinklist.f90
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OPSUM.for -nt ./Release/OPSUM.o ]
then
    echo -e "compiling OPSUM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OPSUM.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/CsvOuts/csvoutput.f90 -nt ./Release/csvoutput.o ]
then
    echo -e "compiling csvoutput"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/CsvOuts/csvoutput.f90
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/WBAL.for -nt ./Release/WBAL.o ]
then
    echo -e "compiling WBAL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/WBAL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/ROOTDM.FOR -nt ./Release/ROOTDM.o ]
then
    echo -e "compiling ROOTDM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/ROOTDM.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/PlantPuptake.for -nt ./Release/PlantPuptake.o ]
then
    echo -e "compiling PlantPuptake"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/PlantPuptake.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OpStemp.for -nt ./Release/OpStemp.o ]
then
    echo -e "compiling OpStemp"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OpStemp.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_GROSUB.for -nt ./Release/MZ_GROSUB.o ]
then
    echo -e "compiling MZ_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_GROSUB.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_GROSUB.FOR -nt ./Release/ML_GROSUB.o ]
then
    echo -e "compiling ML_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_GROSUB.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Weather/HMET.for -nt ./Release/HMET.o ]
then
    echo -e "compiling HMET"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Weather/HMET.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_oppest.for -nt ./Release/for_oppest.o ]
then
    echo -e "compiling for_oppest"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_oppest.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_nfix.for -nt ./Release/for_nfix.o ]
then
    echo -e "compiling for_nfix"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_nfix.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_NFACTO.for -nt ./Release/BS_NFACTO.o ]
then
    echo -e "compiling BS_NFACTO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_NFACTO.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/ASMDM.FOR -nt ./Release/ASMDM.o ]
then
    echo -e "compiling ASMDM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/ASMDM.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR -nt ./Release/SG_NUPTAK.o ]
then
    echo -e "compiling SG_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_THTIME.for -nt ./Release/PT_THTIME.o ]
then
    echo -e "compiling PT_THTIME"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_THTIME.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/OPPEST.FOR -nt ./Release/OPPEST.o ]
then
    echo -e "compiling OPPEST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/OPPEST.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/Opgrow.for -nt ./Release/Opgrow.o ]
then
    echo -e "compiling Opgrow"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/Opgrow.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/OPETPHOT.FOR -nt ./Release/OPETPHOT.o ]
then
    echo -e "compiling OPETPHOT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/OPETPHOT.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_CERES.for -nt ./Release/ML_CERES.o ]
then
    echo -e "compiling ML_CERES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_CERES.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/IPPROG.FOR -nt ./Release/IPPROG.o ]
then
    echo -e "compiling IPPROG"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/IPPROG.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_photo.for -nt ./Release/for_photo.o ]
then
    echo -e "compiling for_photo"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_photo.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ipprog.for -nt ./Release/for_ipprog.o ]
then
    echo -e "compiling for_ipprog"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ipprog.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ch2oref.for -nt ./Release/for_ch2oref.o ]
then
    echo -e "compiling for_ch2oref"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ch2oref.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_PHENOL.for -nt ./Release/BS_PHENOL.o ]
then
    echo -e "compiling BS_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_PHENOL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/Alt_Plant.for -nt ./Release/Alt_Plant.o ]
then
    echo -e "compiling Alt_Plant"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/Alt_Plant.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/TotalP.for -nt ./Release/TotalP.o ]
then
    echo -e "compiling TotalP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/TotalP.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_OPGROW.FOR -nt ./Release/SF_OPGROW.o ]
then
    echo -e "compiling SF_OPGROW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_OPGROW.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_CERES.FOR -nt ./Release/SF_CERES.o ]
then
    echo -e "compiling SF_CERES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_CERES.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/PODDET.FOR -nt ./Release/PODDET.o ]
then
    echo -e "compiling PODDET"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/PODDET.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OpSoilNC.for -nt ./Release/OpSoilNC.o ]
then
    echo -e "compiling OpSoilNC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OpSoilNC.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/NFIX.FOR -nt ./Release/NFIX.o ]
then
    echo -e "compiling NFIX"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/NFIX.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_pest.for -nt ./Release/for_pest.o ]
then
    echo -e "compiling for_pest"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_pest.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ippest.for -nt ./Release/for_ippest.o ]
then
    echo -e "compiling for_ippest"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ippest.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Fertilizerfate.for -nt ./Release/Fertilizerfate.o ]
then
    echo -e "compiling Fertilizerfate"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Fertilizerfate.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_PHENOL.for -nt ./Release/SF_PHENOL.o ]
then
    echo -e "compiling SF_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_PHENOL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/PODS.FOR -nt ./Release/PODS.o ]
then
    echo -e "compiling PODS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/PODS.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/PESTCP.FOR -nt ./Release/PESTCP.o ]
then
    echo -e "compiling PESTCP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/PESTCP.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_NFACTO.for -nt ./Release/MZ_NFACTO.o ]
then
    echo -e "compiling MZ_NFACTO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_NFACTO.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Input/IPVARC.for -nt ./Release/IPVARC.o ]
then
    echo -e "compiling IPVARC"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Input/IPVARC.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_phenol.for -nt ./Release/for_phenol.o ]
then
    echo -e "compiling for_phenol"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_phenol.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_mobil.for -nt ./Release/for_mobil.o ]
then
    echo -e "compiling for_mobil"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_mobil.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/UPDATEPOOL.for -nt ./Release/UPDATEPOOL.o ]
then
    echo -e "compiling UPDATEPOOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/UPDATEPOOL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_OPHARV.FOR -nt ./Release/SG_OPHARV.o ]
then
    echo -e "compiling SG_OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_OPHARV.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/RESPIR.FOR -nt ./Release/RESPIR.o ]
then
    echo -e "compiling RESPIR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/RESPIR.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_OPHARV.for -nt ./Release/PT_OPHARV.o ]
then
    echo -e "compiling PT_OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_OPHARV.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/PEST.FOR -nt ./Release/PEST.o ]
then
    echo -e "compiling PEST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/PEST.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Openfile.for -nt ./Release/Openfile.o ]
then
    echo -e "compiling Openfile"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Openfile.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/IPSOIL.FOR -nt ./Release/IPSOIL.o ]
then
    echo -e "compiling IPSOIL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/IPSOIL.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/Ipphenol.for -nt ./Release/Ipphenol.o ]
then
    echo -e "compiling Ipphenol"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/Ipphenol.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_pods.for -nt ./Release/for_pods.o ]
then
    echo -e "compiling for_pods"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_pods.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ipphenol.for -nt ./Release/for_ipphenol.o ]
then
    echo -e "compiling for_ipphenol"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ipphenol.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_canopy.for -nt ./Release/for_canopy.o ]
then
    echo -e "compiling for_canopy"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_canopy.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/CSCER040.FOR -nt ./Release/CSCER040.o ]
then
    echo -e "compiling CSCER040"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/CSCER040.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Weather/WEATHR.for -nt ./Release/WEATHR.o ]
then
    echo -e "compiling WEATHR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Weather/WEATHR.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/tillage.for -nt ./Release/tillage.o ]
then
    echo -e "compiling tillage"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/tillage.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/SEEDDM.FOR -nt ./Release/SEEDDM.o ]
then
    echo -e "compiling SEEDDM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/SEEDDM.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/RStages.for -nt ./Release/RStages.o ]
then
    echo -e "compiling RStages"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/RStages.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OPVIEW.FOR -nt ./Release/OPVIEW.o ]
then
    echo -e "compiling OPVIEW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OPVIEW.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_OPNIT.FOR -nt ./Release/MZ_OPNIT.o ]
then
    echo -e "compiling MZ_OPNIT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_OPNIT.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/HeatUnit.for -nt ./Release/HeatUnit.o ]
then
    echo -e "compiling HeatUnit"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/HeatUnit.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_rootdm.for -nt ./Release/for_rootdm.o ]
then
    echo -e "compiling for_rootdm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_rootdm.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_opharv.for -nt ./Release/for_opharv.o ]
then
    echo -e "compiling for_opharv"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_opharv.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_freeze.for -nt ./Release/for_freeze.o ]
then
    echo -e "compiling for_freeze"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_freeze.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/DEMAND.FOR -nt ./Release/DEMAND.o ]
then
    echo -e "compiling DEMAND"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/DEMAND.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/CANOPY.FOR -nt ./Release/CANOPY.o ]
then
    echo -e "compiling CANOPY"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/CANOPY.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Breaknode.for -nt ./Release/Breaknode.o ]
then
    echo -e "compiling Breaknode"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Breaknode.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/SENES.FOR -nt ./Release/SENES.o ]
then
    echo -e "compiling SENES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/SENES.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzmain.for -nt ./Release/Rzmain.o ]
then
    echo -e "compiling Rzmain"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzmain.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/PlantNBal.FOR -nt ./Release/PlantNBal.o ]
then
    echo -e "compiling PlantNBal"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/PlantNBal.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Weather/PET.FOR -nt ./Release/PET.o ]
then
    echo -e "compiling PET"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Weather/PET.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_plantnbal.for -nt ./Release/for_plantnbal.o ]
then
    echo -e "compiling for_plantnbal"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_plantnbal.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_hres_cgro.for -nt ./Release/for_hres_cgro.o ]
then
    echo -e "compiling for_hres_cgro"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_hres_cgro.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Writeoutput.for -nt ./Release/Writeoutput.o ]
then
    echo -e "compiling Writeoutput"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Writeoutput.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/UTILS.for -nt ./Release/UTILS.o ]
then
    echo -e "compiling UTILS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/UTILS.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzman.for -nt ./Release/Rzman.o ]
then
    echo -e "compiling Rzman"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzman.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/READS.for -nt ./Release/READS.o ]
then
    echo -e "compiling READS"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/READS.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/PHOTO.FOR -nt ./Release/PHOTO.o ]
then
    echo -e "compiling PHOTO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/PHOTO.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OPWBAL.for -nt ./Release/OPWBAL.o ]
then
    echo -e "compiling OPWBAL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OPWBAL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/NUPTAK.FOR -nt ./Release/NUPTAK.o ]
then
    echo -e "compiling NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/IPPEST.FOR -nt ./Release/IPPEST.o ]
then
    echo -e "compiling IPPEST"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/IPPEST.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_seeddm.for -nt ./Release/for_seeddm.o ]
then
    echo -e "compiling for_seeddm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_seeddm.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_incomp.for -nt ./Release/for_incomp.o ]
then
    echo -e "compiling for_incomp"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_incomp.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_asmdm.for -nt ./Release/for_asmdm.o ]
then
    echo -e "compiling for_asmdm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_asmdm.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Cropdaycheck.for -nt ./Release/Cropdaycheck.o ]
then
    echo -e "compiling Cropdaycheck"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Cropdaycheck.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_GROSUB.for -nt ./Release/BS_GROSUB.o ]
then
    echo -e "compiling BS_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_GROSUB.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/SPSUBS.for -nt ./Release/SPSUBS.o ]
then
    echo -e "compiling SPSUBS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/SPSUBS.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/SoilNBal.FOR -nt ./Release/SoilNBal.o ]
then
    echo -e "compiling SoilNBal"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/SoilNBal.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/ROOTWU.FOR -nt ./Release/ROOTWU.o ]
then
    echo -e "compiling ROOTWU"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/ROOTWU.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_PHENOL.for -nt ./Release/PT_PHENOL.o ]
then
    echo -e "compiling PT_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_PHENOL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/OPHARV.FOR -nt ./Release/OPHARV.o ]
then
    echo -e "compiling OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/OPHARV.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_CERES.FOR -nt ./Release/MZ_CERES.o ]
then
    echo -e "compiling MZ_CERES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_CERES.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_opharv.for -nt ./Release/ML_opharv.o ]
then
    echo -e "compiling ML_opharv"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_opharv.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Initializepvar.for -nt ./Release/Initializepvar.o ]
then
    echo -e "compiling Initializepvar"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Initializepvar.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_veggr.for -nt ./Release/for_veggr.o ]
then
    echo -e "compiling for_veggr"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_veggr.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_respir.for -nt ./Release/for_respir.o ]
then
    echo -e "compiling for_respir"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_respir.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_opgrow.for -nt ./Release/for_opgrow.o ]
then
    echo -e "compiling for_opgrow"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_opgrow.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Fertilizer.for -nt ./Release/Fertilizer.o ]
then
    echo -e "compiling Fertilizer"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Fertilizer.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR -nt ./Release/BS_OPHARV.o ]
then
    echo -e "compiling BS_OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR -nt ./Release/BS_OPGROW.o ]
then
    echo -e "compiling BS_OPGROW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/VEGGR.FOR -nt ./Release/VEGGR.o ]
then
    echo -e "compiling VEGGR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/VEGGR.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_OPHARV.FOR -nt ./Release/SF_OPHARV.o ]
then
    echo -e "compiling SF_OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_OPHARV.FOR
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rzday.for -nt ./Release/Rzday.o ]
then
    echo -e "compiling Rzday"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rzday.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for -nt ./Release/PT_ROOTGR.o ]
then
    echo -e "compiling PT_ROOTGR"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_TILLSUB.FOR -nt ./Release/ML_TILLSUB.o ]
then
    echo -e "compiling ML_TILLSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_TILLSUB.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Labploss.for -nt ./Release/Labploss.o ]
then
    echo -e "compiling Labploss"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Labploss.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/GROW.FOR -nt ./Release/GROW.o ]
then
    echo -e "compiling GROW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/GROW.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_roots.for -nt ./Release/for_roots.o ]
then
    echo -e "compiling for_roots"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_roots.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/DSSATDRV.for -nt ./Release/DSSATDRV.o ]
then
    echo -e "compiling DSSATDRV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/DSSATDRV.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/CSCERES_Interface.for -nt ./Release/CSCERES_Interface.o ]
then
    echo -e "compiling CSCERES_Interface"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/CSCERES_Interface.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Writelog.for -nt ./Release/Writelog.o ]
then
    echo -e "compiling Writelog"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Writelog.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_CERES.for -nt ./Release/SG_CERES.o ]
then
    echo -e "compiling SG_CERES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_CERES.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_ROOTS.FOR -nt ./Release/SF_ROOTS.o ]
then
    echo -e "compiling SF_ROOTS"

	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_ROOTS.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_GROSUB.for -nt ./Release/PT_GROSUB.o ]
then
    echo -e "compiling PT_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_GROSUB.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Weather/OPWEATH.FOR -nt ./Release/OPWEATH.o ]
then
    echo -e "compiling OPWEATH"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Weather/OPWEATH.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_NUPTAK.FOR -nt ./Release/MZ_NUPTAK.o ]
then
    echo -e "compiling MZ_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/forage.for -nt ./Release/forage.o ]
then
    echo -e "compiling forage"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/forage.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_grow.for -nt ./Release/for_grow.o ]
then
    echo -e "compiling for_grow"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_grow.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Drplossrunoff.for -nt ./Release/Drplossrunoff.o ]
then
    echo -e "compiling Drplossrunoff"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Drplossrunoff.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/CROPGRO.for -nt ./Release/CROPGRO.o ]
then
    echo -e "compiling CROPGRO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/CROPGRO.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sorghum/SG_GROSUB.FOR -nt ./Release/SG_GROSUB.o ]
then
    echo -e "compiling SG_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sorghum/SG_GROSUB.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_GROSUB.for -nt ./Release/SF_GROSUB.o ]
then
    echo -e "compiling SF_GROSUB"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_GROSUB.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/ROOTS.for -nt ./Release/ROOTS.o ]
then
    echo -e "compiling ROOTS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/ROOTS.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR -nt ./Release/PT_SUBSTOR.o ]
then
    echo -e "compiling PT_SUBSTOR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/PHENOL.FOR -nt ./Release/PHENOL.o ]
then
    echo -e "compiling PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/PHENOL.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/MOBIL.FOR -nt ./Release/MOBIL.o ]
then
    echo -e "compiling MOBIL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/MOBIL.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Millet/ML_NUPTAK.FOR -nt ./Release/ML_NUPTAK.o ]
then
    echo -e "compiling ML_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Millet/ML_NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CSCER/HResCeres.for -nt ./Release/HResCeres.o ]
then
    echo -e "compiling HResCeres"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CSCER/HResCeres.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_pestcp.for -nt ./Release/for_pestcp.o ]
then
    echo -e "compiling for_pestcp"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_pestcp.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_nuptak.for -nt ./Release/for_nuptak.o ]
then
    echo -e "compiling for_nuptak"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_nuptak.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/DATES.FOR -nt ./Release/DATES.o ]
then
    echo -e "compiling DATES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/DATES.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR -nt ./Release/BS_NUPTAK.o ]
then
    echo -e "compiling BS_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/AUTHAR.FOR -nt ./Release/AUTHAR.o ]
then
    echo -e "compiling AUTHAR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/AUTHAR.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Generic-Pest/VEGDM.FOR -nt ./Release/VEGDM.o ]
then
    echo -e "compiling VEGDM"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Generic-Pest/VEGDM.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_OPGROW.for -nt ./Release/PT_OPGROW.o ]
then
    echo -e "compiling PT_OPGROW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_OPGROW.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/PPlosstile.for -nt ./Release/PPlosstile.o ]
then
    echo -e "compiling PPlosstile"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/PPlosstile.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Input/OPGEN.FOR -nt ./Release/OPGEN.o ]
then
    echo -e "compiling OPGEN"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Input/OPGEN.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_PHENOL.for -nt ./Release/MZ_PHENOL.o ]
then
    echo -e "compiling MZ_PHENOL"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_PHENOL.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/IPPLNT.FOR -nt ./Release/IPPLNT.o ]
then
    echo -e "compiling IPPLNT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/IPPLNT.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_vegdm.for -nt ./Release/for_vegdm.o ]
then
    echo -e "compiling for_vegdm"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_vegdm.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_harv.for -nt ./Release/for_harv.o ]
then
    echo -e "compiling for_harv"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_harv.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/ERROR.for -nt ./Release/ERROR.o ]
then
    echo -e "compiling ERROR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/ERROR.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR -nt ./Release/BS_ROOTS.o ]
then
    echo -e "compiling BS_ROOTS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Writeheader.for -nt ./Release/Writeheader.o ]
then
    echo -e "compiling Writeheader"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Writeheader.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_OPNIT.FOR -nt ./Release/SF_OPNIT.o ]
then
    echo -e "compiling SF_OPNIT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_OPNIT.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/PBALANCE.for -nt ./Release/PBALANCE.o ]
then
    echo -e "compiling PBALANCE"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/PBALANCE.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Manurefate.for -nt ./Release/Manurefate.o ]
then
    echo -e "compiling Manurefate"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Manurefate.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_rstages.for -nt ./Release/for_rstages.o ]
then
    echo -e "compiling for_rstages"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_rstages.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_opmob.for -nt ./Release/for_opmob.o ]
then
    echo -e "compiling for_opmob"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_opmob.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_demand.for -nt ./Release/for_demand.o ]
then
    echo -e "compiling for_demand"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_demand.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Drplosstile.for -nt ./Release/Drplosstile.o ]
then
    echo -e "compiling Drplosstile"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Drplosstile.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_CERES.FOR -nt ./Release/BS_CERES.o ]
then
    echo -e "compiling BS_CERES"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_CERES.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/Warning.FOR -nt ./Release/Warning.o ]
then
    echo -e "compiling Warning"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/Warning.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/PPlossrunoff.for -nt ./Release/PPlossrunoff.o ]
then
    echo -e "compiling PPlossrunoff"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/PPlossrunoff.for
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Pflux.for -nt ./Release/Pflux.o ]
then
    echo -e "compiling Pflux"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Pflux.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_ROOTS.FOR -nt ./Release/MZ_ROOTS.o ]
then
    echo -e "compiling MZ_ROOTS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_ROOTS.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_OPGROW.FOR -nt ./Release/MZ_OPGROW.o ]
then
    echo -e "compiling MZ_OPGROW"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_OPGROW.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/ManureP.for -nt ./Release/ManureP.o ]
then
    echo -e "compiling ManureP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/ManureP.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/HRes_CGRO.for -nt ./Release/HRes_CGRO.o ]
then
    echo -e "compiling HRes_CGRO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/HRes_CGRO.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_poddet.for -nt ./Release/for_poddet.o ]
then
    echo -e "compiling for_poddet"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_poddet.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/ETPHR.for -nt ./Release/ETPHR.o ]
then
    echo -e "compiling ETPHR"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/ETPHR.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/TRANS.FOR -nt ./Release/TRANS.o ]
then
    echo -e "compiling TRANS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/TRANS.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Input/OPHEAD.FOR -nt ./Release/OPHEAD.o ]
then
    echo -e "compiling OPHEAD"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Input/OPHEAD.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Maize/MZ_OPHARV.FOR -nt ./Release/MZ_OPHARV.o ]
then
    echo -e "compiling MZ_OPHARV"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Maize/MZ_OPHARV.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/INCOMP.FOR -nt ./Release/INCOMP.o ]
then
    echo -e "compiling INCOMP"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/INCOMP.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_senmob.for -nt ./Release/for_senmob.o ]
then
    echo -e "compiling for_senmob"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_senmob.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_ipplnt.for -nt ./Release/for_ipplnt.o ]
then
    echo -e "compiling for_ipplnt"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_ipplnt.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_HResCeres.for -nt ./Release/BS_HResCeres.o ]
then
    echo -e "compiling BS_HResCeres"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_HResCeres.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR -nt ./Release/SF_NUPTAK.o ]
then
    echo -e "compiling SF_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sunflower/SF_NFACTO.for -nt ./Release/SF_NFACTO.o ]
then
    echo -e "compiling SF_NFACTO"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sunflower/SF_NFACTO.for
fi
if [ "$FORCE" = true ] || [ ./RZWQM/Rznutr.for -nt ./Release/Rznutr.o ]
then
    echo -e "compiling Rznutr"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./RZWQM/Rznutr.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for -nt ./Release/PT_NUPTAK.o ]
then
    echo -e "compiling PT_NUPTAK"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Utilities/OPSTRESS.FOR -nt ./Release/OPSTRESS.o ]
then
    echo -e "compiling OPSTRESS"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Utilities/OPSTRESS.FOR
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_opview.for -nt ./Release/for_opview.o ]
then
    echo -e "compiling for_opview"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_opview.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_dormancy.for -nt ./Release/for_dormancy.o ]
then
    echo -e "compiling for_dormancy"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_dormancy.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/Forage/for_dormancy.for -nt ./Release/for_dormancy.o ]
then
    echo -e "compiling for_dormancy"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/Forage/for_dormancy.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CROPGRO/ETPHOT.for -nt ./Release/ETPHOT.o ]
then
    echo -e "compiling ETPHOT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CROPGRO/ETPHOT.for
fi
if [ "$FORCE" = true ] || [ ./DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR -nt ./Release/BS_OPNIT.o ]
then
    echo -e "compiling BS_OPNIT"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR
fi
if [ "$FORCE" = true ] || [ ./Pmodel/Addnode.for -nt ./Release/Addnode.o ]
then
    echo -e "compiling Addnode"
	ifort -nologo -fpp -shared-intel -save -zero -module Release/ -FoRelease/ -IRelease/ -module Release/ -static -threads -f77rtl -c ./Pmodel/Addnode.for
fi

echo -e "Now creating the final executable"

ifort -o ./Release/RZlinux -threads -static Release/SHAW_WBCAN.o Release/SHAW_Photosyn.o Release/SHAW_LWRBAL.o Release/SG_PHENOL.o Release/Rzpet.o Release/DSSATNFIX.o Release/SHAW_VSLOPE.o Release/SHAW_TRANSC.o Release/SHAW_RESVK.o Release/SHAW_META.o Release/PT_NFACTO.o Release/HERMES_Radia.o Release/SHAW_Weight.o Release/SHAW_SOILR.o Release/SHAW_SNOWHT.o Release/SHAW_LEAFT.o Release/SHAW_Conduc.o Release/Rzchem.o Release/KghaToKg.o Release/FREEZE.o Release/Comp2exp.o Release/SOLAR.o Release/SHAW_SWRBAL.o Release/SHAW_FSLOPE.o Release/SHAW_Frozen.o Release/SHAW_CANHUM.o Release/Qckturf.o Release/HERMES_SetupCrop.o Release/CSUTS040.o Release/SHAW_TRANSR.o Release/SHAW_LWRMAT.o Release/SHAW_LWRATM.o Release/SHAW_Canlay.o Release/SHAW_Solvrad.o Release/SHAW_SNOWBC.o Release/SHAW_NWSNOW.o Release/SHAW_EBCAN.o Release/Qckplnt.o Release/ML_PHASEI.o Release/SHAW_SWRSNO.o Release/SHAW_STMMLT.o Release/SHAW_RESGMC.o Release/SHAW_GOSHAW.o Release/SHAW_ADJUST.o Release/LINDM.o Release/Horizon.o Release/CSREA040.o Release/SHAW_SUMDT.o Release/SHAW_SNOALB.o Release/SHAW_RESHT.o Release/SG_ROOTGR.o Release/SG_NFACT.o Release/ML_rootgr.o Release/SHAW_Solar.o Release/SHAW_RESTK.o Release/SHAW_EBSNOW.o Release/Rzplnt.o Release/SHAW_STAB.o Release/SHAW_QVSOIL.o Release/SHAW_EBSOIL.o Release/SHAW_Cloudy.o Release/for_lindm.o Release/SHAW_WBALNC.o Release/SHAW_source.o Release/SHAW_SOILHT.o Release/SHAW_LWRRES.o Release/Rzpest.o Release/Qcktree.o Release/ML_PHENOL.o Release/for_sdcomp.o Release/SHAW_WBSNOW.o Release/SHAW_SWRCAN.o Release/SHAW_SOILTK.o Release/SHAW_RESVAP.o Release/SHAW_TDMA.o Release/SHAW_QVSNOW.o Release/SHAW_LWRSNO.o Release/SHAW_BACKUP.o Release/SG_PHASEI.o Release/KgtoKgha.o Release/SHAW_SNOMLT.o Release/SHAW_QVRES.o Release/Rzrich.o Release/Rzout.o Release/readrzx.o Release/Nodethick.o Release/IPPARM.o Release/SHAW_WBRES.o Release/SHAW_RESHUM.o Release/SHAW_FROST.o Release/SDCOMP.o Release/RZTEST.o Release/REF_ET.o Release/for_ipparm.o Release/SHAW_Update.o Release/SHAW_RTDIST.o Release/SHAW_ENHANC.o Release/SHAW_ATstab.o Release/RZTEMP.o Release/RZ-Erosion.o Release/PT_PHASEI.o Release/ML_NFACT.o Release/Countdata.o Release/Snowprms.o Release/SHAW_SNOWTK.o Release/SHAW_LWRCAN.o Release/SHAW_EBRES.o Release/NO_N2O_RATIO.o Release/LMATCH.o Release/HERMES_SUCROS.o Release/ModuleDefs.o Release/Variable.o Release/csvlinklist.o Release/OPSUM.o Release/csvoutput.o Release/WBAL.o Release/ROOTDM.o Release/PlantPuptake.o Release/OpStemp.o Release/MZ_GROSUB.o Release/ML_GROSUB.o Release/HMET.o Release/for_oppest.o Release/for_nfix.o Release/BS_NFACTO.o Release/ASMDM.o Release/SG_NUPTAK.o Release/PT_THTIME.o Release/OPPEST.o Release/Opgrow.o Release/OPETPHOT.o Release/ML_CERES.o Release/IPPROG.o Release/for_photo.o Release/for_ipprog.o Release/for_ch2oref.o Release/BS_PHENOL.o Release/Alt_Plant.o Release/TotalP.o Release/SF_OPGROW.o Release/SF_CERES.o Release/PODDET.o Release/OpSoilNC.o Release/NFIX.o Release/for_pest.o Release/for_ippest.o Release/Fertilizerfate.o Release/SF_PHENOL.o Release/PODS.o Release/PESTCP.o Release/MZ_NFACTO.o Release/IPVARC.o Release/for_phenol.o Release/for_mobil.o Release/UPDATEPOOL.o Release/SG_OPHARV.o Release/RESPIR.o Release/PT_OPHARV.o Release/PEST.o Release/Openfile.o Release/IPSOIL.o Release/Ipphenol.o Release/for_pods.o Release/for_ipphenol.o Release/for_canopy.o Release/CSCER040.o Release/WEATHR.o Release/tillage.o Release/SEEDDM.o Release/RStages.o Release/OPVIEW.o Release/MZ_OPNIT.o Release/HeatUnit.o Release/for_rootdm.o Release/for_opharv.o Release/for_freeze.o Release/DEMAND.o Release/CANOPY.o Release/Breaknode.o Release/SENES.o Release/Rzmain.o Release/PlantNBal.o Release/PET.o Release/for_plantnbal.o Release/for_hres_cgro.o Release/Writeoutput.o Release/UTILS.o Release/Rzman.o Release/READS.o Release/PHOTO.o Release/OPWBAL.o Release/NUPTAK.o Release/IPPEST.o Release/for_seeddm.o Release/for_incomp.o Release/for_asmdm.o Release/Cropdaycheck.o Release/BS_GROSUB.o Release/SPSUBS.o Release/SoilNBal.o Release/ROOTWU.o Release/PT_PHENOL.o Release/OPHARV.o Release/MZ_CERES.o Release/ML_opharv.o Release/Initializepvar.o Release/for_veggr.o Release/for_respir.o Release/for_opgrow.o Release/Fertilizer.o Release/BS_OPHARV.o Release/BS_OPGROW.o Release/VEGGR.o Release/SF_OPHARV.o Release/Rzday.o Release/PT_ROOTGR.o Release/ML_TILLSUB.o Release/Labploss.o Release/GROW.o Release/for_roots.o Release/DSSATDRV.o Release/CSCERES_Interface.o Release/Writelog.o Release/SG_CERES.o Release/SF_ROOTS.o Release/PT_GROSUB.o Release/OPWEATH.o Release/MZ_NUPTAK.o Release/forage.o Release/for_grow.o Release/Drplossrunoff.o Release/CROPGRO.o Release/SG_GROSUB.o Release/SF_GROSUB.o Release/ROOTS.o Release/PT_SUBSTOR.o Release/PHENOL.o Release/MOBIL.o Release/ML_NUPTAK.o Release/HResCeres.o Release/for_pestcp.o Release/for_nuptak.o Release/DATES.o Release/BS_NUPTAK.o Release/AUTHAR.o Release/VEGDM.o Release/PT_OPGROW.o Release/PPlosstile.o Release/OPGEN.o Release/MZ_PHENOL.o Release/IPPLNT.o Release/for_vegdm.o Release/for_harv.o Release/ERROR.o Release/BS_ROOTS.o Release/Writeheader.o Release/SF_OPNIT.o Release/PBALANCE.o Release/Manurefate.o Release/for_rstages.o Release/for_opmob.o Release/for_demand.o Release/Drplosstile.o Release/BS_CERES.o Release/Warning.o Release/PPlossrunoff.o Release/Pflux.o Release/MZ_ROOTS.o Release/MZ_OPGROW.o Release/ManureP.o Release/HRes_CGRO.o Release/for_poddet.o Release/ETPHR.o Release/TRANS.o Release/OPHEAD.o Release/MZ_OPHARV.o Release/INCOMP.o Release/for_senmob.o Release/for_ipplnt.o Release/BS_HResCeres.o Release/SF_NUPTAK.o Release/SF_NFACTO.o Release/Rznutr.o Release/PT_NUPTAK.o Release/OPSTRESS.o Release/for_opview.o Release/for_dormancy.o Release/ETPHOT.o Release/BS_OPNIT.o Release/Addnode.o

