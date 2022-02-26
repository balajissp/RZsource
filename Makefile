SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c  

# https://stackoverflow.com/a/27164688
ifeq ($(ENV),ifort)
 F77 = ifort
 F77_FLAGGED=$(F77) -nologo -fpp -shared-intel -save -zero -module $(ODIR)/ -Fo$(ODIR)/ -I$(ODIR)/ -module $(ODIR)/ -static -threads -f77rtl
 F77_LINKER=$(F77) -threads -static
 ODIR = ./IntelBuild
else
 F77 = /usr/bin/gfortran
 F77_FLAGGED=$(F77) -cpp -finit-local-zero -fintrinsic-modules-path $(ODIR)/ -J$(ODIR)/ -I$(ODIR)/ -fopenmp -ff2c 
 F77_LINKER=$(F77) -mthreads -static
 ODIR = ./GNUBuild
endif
# F77_FLAGGED=$(F77) -nologo -fpp -shared-intel -save -zero -module $(ODIR)/ -Fo$(ODIR)/ -I$(ODIR)/ -module $(ODIR)/ -static -threads -f77rtl
# F77_FLAGGED=$(F77) -cpp -finit-local-zero -fintrinsic-modules-path $(ODIR)/ -J$(ODIR)/ -I$(ODIR)/ -fopenmp -ff2c 


# _OBJ = SHAW24b/SHAW_WBCAN.FOR SHAW24b/SHAW_Photosyn.for SHAW24b/SHAW_LWRBAL.FOR DSSAT40/CERES-Sorghum/SG_PHENOL.FOR RZWQM/Rzpet.for RZWQM/DSSATNFIX.FOR SHAW24b/SHAW_VSLOPE.FOR SHAW24b/SHAW_TRANSC.FOR SHAW24b/SHAW_RESVK.FOR SHAW24b/SHAW_META.FOR SHAW24b/SHAW_CANTK.For DSSAT40/SUBSTOR-Potato/PT_NFACTO.for RZWQM/HERMES_Radia.for SHAW24b/SHAW_Weight.for SHAW24b/SHAW_SOILR.FOR SHAW24b/SHAW_SNOWHT.FOR SHAW24b/SHAW_LEAFT.for SHAW24b/SHAW_Conduc.for RZWQM/Rzchem.for Pmodel/KghaToKg.for DSSAT40/CROPGRO/FREEZE.FOR RZWQM/Comp2exp.for DSSAT40/Weather/SOLAR.FOR SHAW24b/SHAW_SWRBAL.FOR SHAW24b/SHAW_FSLOPE.FOR SHAW24b/SHAW_Frozen.for SHAW24b/SHAW_CANHUM.FOR RZWQM/Qckturf.for RZWQM/HERMES_SetupCrop.for DSSAT40/CSCER/CSUTS040.FOR SHAW24b/SHAW_TRANSR.FOR SHAW24b/SHAW_LWRMAT.FOR SHAW24b/SHAW_LWRATM.FOR SHAW24b/SHAW_Canlay.for SHAW24b/SHAW_Solvrad.for SHAW24b/SHAW_SNOWBC.FOR SHAW24b/SHAW_NWSNOW.FOR SHAW24b/SHAW_EBCAN.FOR RZWQM/Qckplnt.for DSSAT40/CERES-Millet/ML_PHASEI.FOR SHAW24b/SHAW_SWRSNO.FOR SHAW24b/SHAW_STMMLT.FOR SHAW24b/SHAW_RESGMC.FOR SHAW24b/SHAW_GOSHAW.FOR SHAW24b/SHAW_ADJUST.FOR DSSAT40/Generic-Pest/LINDM.FOR RZWQM/Horizon.for DSSAT40/CSCER/CSREA040.FOR SHAW24b/SHAW_SUMDT.FOR SHAW24b/SHAW_SNOALB.FOR SHAW24b/SHAW_RESHT.FOR DSSAT40/CERES-Sorghum/SG_ROOTGR.for DSSAT40/CERES-Sorghum/SG_NFACT.FOR DSSAT40/CERES-Millet/ML_rootgr.for SHAW24b/SHAW_Solar.for SHAW24b/SHAW_RESTK.FOR SHAW24b/SHAW_EBSNOW.FOR RZWQM/Rzplnt.for SHAW24b/SHAW_STAB.FOR SHAW24b/SHAW_QVSOIL.FOR SHAW24b/SHAW_EBSOIL.FOR SHAW24b/SHAW_Cloudy.for DSSAT40/Forage/for_lindm.for SHAW24b/SHAW_WBALNC.FOR SHAW24b/SHAW_source.for SHAW24b/SHAW_SOILHT.FOR SHAW24b/SHAW_LWRRES.FOR RZWQM/Rzpest.for RZWQM/Qcktree.for DSSAT40/CERES-Millet/ML_PHENOL.FOR DSSAT40/Forage/for_sdcomp.for SHAW24b/SHAW_WBSNOW.FOR SHAW24b/SHAW_SWRCAN.FOR SHAW24b/SHAW_SOILTK.FOR SHAW24b/SHAW_RESVAP.FOR SHAW24b/SHAW_TDMA.FOR SHAW24b/SHAW_QVSNOW.FOR SHAW24b/SHAW_LWRSNO.FOR SHAW24b/SHAW_BACKUP.FOR DSSAT40/CERES-Sorghum/SG_PHASEI.FOR Pmodel/KgtoKgha.for SHAW24b/SHAW_SNOMLT.FOR SHAW24b/SHAW_QVRES.FOR RZWQM/Rzrich.for RZWQM/Rzout.for RZWQM/readrzx.for Pmodel/Nodethick.for DSSAT40/Generic-Pest/IPPARM.for SHAW24b/SHAW_WBRES.FOR SHAW24b/SHAW_RESHUM.FOR SHAW24b/SHAW_FROST.FOR DSSAT40/CROPGRO/SDCOMP.FOR RZWQM/RZTEST.for RZWQM/REF_ET.FOR DSSAT40/Forage/for_ipparm.for SHAW24b/SHAW_Update.for SHAW24b/SHAW_RTDIST.for SHAW24b/SHAW_ENHANC.FOR SHAW24b/SHAW_ATstab.for RZWQM/RZTEMP.FOR GLEAMS/RZ-Erosion.for DSSAT40/SUBSTOR-Potato/PT_PHASEI.for DSSAT40/CERES-Millet/ML_NFACT.FOR Pmodel/Countdata.for RZWQM/Snowprms.for SHAW24b/SHAW_SNOWTK.FOR SHAW24b/SHAW_LWRCAN.FOR SHAW24b/SHAW_EBRES.FOR RZWQM/NO_N2O_RATIO.FOR DSSAT40/Input/LMATCH.FOR RZWQM/HERMES_SUCROS.for DSSAT40/Utilities/ModuleDefs.for Pmodel/Variable.for DSSAT40/Utilities/CsvOuts/csvlinklist.f90 DSSAT40/Utilities/OPSUM.for DSSAT40/Utilities/CsvOuts/csvoutput.f90 DSSAT40/Utilities/WBAL.for DSSAT40/Generic-Pest/ROOTDM.FOR Pmodel/PlantPuptake.for DSSAT40/Utilities/OpStemp.for DSSAT40/CERES-Maize/MZ_GROSUB.for DSSAT40/CERES-Millet/ML_GROSUB.FOR DSSAT40/Weather/HMET.for DSSAT40/Forage/for_oppest.for DSSAT40/Forage/for_nfix.for DSSAT40/CERES-Sugarbeet/BS_NFACTO.for DSSAT40/Generic-Pest/ASMDM.FOR DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR DSSAT40/SUBSTOR-Potato/PT_THTIME.for DSSAT40/Generic-Pest/OPPEST.FOR DSSAT40/CROPGRO/Opgrow.for DSSAT40/CROPGRO/OPETPHOT.FOR DSSAT40/CERES-Millet/ML_CERES.for DSSAT40/Generic-Pest/IPPROG.FOR DSSAT40/Forage/for_photo.for DSSAT40/Forage/for_ipprog.for DSSAT40/Forage/for_ch2oref.for DSSAT40/CERES-Sugarbeet/BS_PHENOL.for DSSAT40/CSCER/Alt_Plant.for Pmodel/TotalP.for DSSAT40/CERES-Sunflower/SF_OPGROW.FOR DSSAT40/CERES-Sunflower/SF_CERES.FOR DSSAT40/CROPGRO/PODDET.FOR DSSAT40/Utilities/OpSoilNC.for DSSAT40/CROPGRO/NFIX.FOR DSSAT40/Forage/for_pest.for DSSAT40/Forage/for_ippest.for Pmodel/Fertilizerfate.for DSSAT40/CERES-Sunflower/SF_PHENOL.for DSSAT40/CROPGRO/PODS.FOR DSSAT40/Generic-Pest/PESTCP.FOR DSSAT40/CERES-Maize/MZ_NFACTO.for DSSAT40/Input/IPVARC.for DSSAT40/Forage/for_phenol.for DSSAT40/Forage/for_mobil.for Pmodel/UPDATEPOOL.for DSSAT40/CERES-Sorghum/SG_OPHARV.FOR DSSAT40/CROPGRO/RESPIR.FOR DSSAT40/SUBSTOR-Potato/PT_OPHARV.for DSSAT40/Generic-Pest/PEST.FOR Pmodel/Openfile.for DSSAT40/Utilities/IPSOIL.FOR DSSAT40/CROPGRO/Ipphenol.for DSSAT40/Forage/for_pods.for DSSAT40/Forage/for_ipphenol.for DSSAT40/Forage/for_canopy.for DSSAT40/CSCER/CSCER040.FOR DSSAT40/Weather/WEATHR.for Pmodel/tillage.for DSSAT40/Generic-Pest/SEEDDM.FOR DSSAT40/CROPGRO/RStages.for DSSAT40/Utilities/OPVIEW.FOR DSSAT40/CERES-Maize/MZ_OPNIT.FOR Pmodel/HeatUnit.for DSSAT40/Forage/for_rootdm.for DSSAT40/Forage/for_opharv.for DSSAT40/Forage/for_freeze.for DSSAT40/CROPGRO/DEMAND.FOR DSSAT40/CROPGRO/CANOPY.FOR Pmodel/Breaknode.for DSSAT40/CROPGRO/SENES.FOR RZWQM/Rzmain.for DSSAT40/CROPGRO/PlantNBal.FOR DSSAT40/Weather/PET.FOR DSSAT40/Forage/for_plantnbal.for DSSAT40/Forage/for_hres_cgro.for Pmodel/Writeoutput.for DSSAT40/Utilities/UTILS.for RZWQM/Rzman.for DSSAT40/Utilities/READS.for DSSAT40/CROPGRO/PHOTO.FOR DSSAT40/Utilities/OPWBAL.for DSSAT40/CROPGRO/NUPTAK.FOR DSSAT40/Generic-Pest/IPPEST.FOR DSSAT40/Forage/for_seeddm.for DSSAT40/Forage/for_incomp.for DSSAT40/Forage/for_asmdm.for Pmodel/Cropdaycheck.for DSSAT40/CERES-Sugarbeet/BS_GROSUB.for DSSAT40/Utilities/SPSUBS.for DSSAT40/Utilities/SoilNBal.FOR DSSAT40/Utilities/ROOTWU.FOR DSSAT40/SUBSTOR-Potato/PT_PHENOL.for DSSAT40/CROPGRO/OPHARV.FOR DSSAT40/CERES-Maize/MZ_CERES.FOR DSSAT40/CERES-Millet/ML_opharv.for Pmodel/Initializepvar.for DSSAT40/Forage/for_veggr.for DSSAT40/Forage/for_respir.for DSSAT40/Forage/for_opgrow.for Pmodel/Fertilizer.for DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR DSSAT40/CROPGRO/VEGGR.FOR DSSAT40/CERES-Sunflower/SF_OPHARV.FOR RZWQM/Rzday.for DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for DSSAT40/CERES-Millet/ML_TILLSUB.FOR Pmodel/Labploss.for DSSAT40/CROPGRO/GROW.FOR DSSAT40/Forage/for_roots.for RZWQM/DSSATDRV.for DSSAT40/CSCER/CSCERES_Interface.for Pmodel/Writelog.for DSSAT40/CERES-Sorghum/SG_CERES.for DSSAT40/CERES-Sunflower/SF_ROOTS.FOR DSSAT40/SUBSTOR-Potato/PT_GROSUB.for DSSAT40/Weather/OPWEATH.FOR DSSAT40/CERES-Maize/MZ_NUPTAK.FOR DSSAT40/Forage/forage.for DSSAT40/Forage/for_grow.for Pmodel/Drplossrunoff.for DSSAT40/CROPGRO/CROPGRO.for DSSAT40/CERES-Sorghum/SG_GROSUB.FOR DSSAT40/CERES-Sunflower/SF_GROSUB.for DSSAT40/CROPGRO/ROOTS.for DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR DSSAT40/CROPGRO/PHENOL.FOR DSSAT40/CROPGRO/MOBIL.FOR DSSAT40/CERES-Millet/ML_NUPTAK.FOR DSSAT40/CSCER/HResCeres.for DSSAT40/Forage/for_pestcp.for DSSAT40/Forage/for_nuptak.for DSSAT40/Utilities/DATES.FOR DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR DSSAT40/Utilities/AUTHAR.FOR DSSAT40/Generic-Pest/VEGDM.FOR DSSAT40/SUBSTOR-Potato/PT_OPGROW.for Pmodel/PPlosstile.for DSSAT40/Input/OPGEN.FOR DSSAT40/CERES-Maize/MZ_PHENOL.for DSSAT40/CROPGRO/IPPLNT.FOR DSSAT40/Forage/for_vegdm.for DSSAT40/Forage/for_harv.for DSSAT40/Utilities/ERROR.for DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR Pmodel/Writeheader.for DSSAT40/CERES-Sunflower/SF_OPNIT.FOR Pmodel/PBALANCE.for Pmodel/Manurefate.for DSSAT40/Forage/for_rstages.for DSSAT40/Forage/for_opmob.for DSSAT40/Forage/for_demand.for Pmodel/Drplosstile.for DSSAT40/CERES-Sugarbeet/BS_CERES.FOR DSSAT40/Utilities/Warning.FOR Pmodel/PPlossrunoff.for Pmodel/Pflux.for DSSAT40/CERES-Maize/MZ_ROOTS.FOR DSSAT40/CERES-Maize/MZ_OPGROW.FOR Pmodel/ManureP.for DSSAT40/CROPGRO/HRes_CGRO.for DSSAT40/Forage/for_poddet.for DSSAT40/CROPGRO/ETPHR.for DSSAT40/Utilities/TRANS.FOR DSSAT40/Input/OPHEAD.FOR DSSAT40/CERES-Maize/MZ_OPHARV.FOR DSSAT40/CROPGRO/INCOMP.FOR DSSAT40/Forage/for_senmob.for DSSAT40/Forage/for_ipplnt.for DSSAT40/CERES-Sugarbeet/BS_HResCeres.for DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR DSSAT40/CERES-Sunflower/SF_NFACTO.for RZWQM/Rznutr.for DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for DSSAT40/Utilities/OPSTRESS.FOR DSSAT40/Forage/for_opview.for DSSAT40/Forage/for_dormancy.for DSSAT40/Forage/for_dormancy.for DSSAT40/CROPGRO/ETPHOT.for DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR Pmodel/Addnode.for
# # OBJ = $(patsubst %/,$(ODIR)/,$(_OBJ))

# all: $(_OBJ)
# 	$(F77_FLAGGED) -c $^ -o $(ODIR)/$(basename $(notdir $^))
# .PHONY: all

$(ODIR)/RZlinux: $(ODIR)/SHAW_WBCAN.o $(ODIR)/SHAW_Photosyn.o $(ODIR)/SHAW_LWRBAL.o $(ODIR)/SG_PHENOL.o $(ODIR)/Rzpet.o $(ODIR)/DSSATNFIX.o $(ODIR)/SHAW_VSLOPE.o $(ODIR)/SHAW_TRANSC.o $(ODIR)/SHAW_RESVK.o $(ODIR)/SHAW_META.o $(ODIR)/PT_NFACTO.o $(ODIR)/HERMES_Radia.o $(ODIR)/SHAW_Weight.o $(ODIR)/SHAW_SOILR.o $(ODIR)/SHAW_SNOWHT.o $(ODIR)/SHAW_LEAFT.o $(ODIR)/SHAW_Conduc.o $(ODIR)/Rzchem.o $(ODIR)/KghaToKg.o $(ODIR)/FREEZE.o $(ODIR)/Comp2exp.o $(ODIR)/SOLAR.o $(ODIR)/SHAW_SWRBAL.o $(ODIR)/SHAW_FSLOPE.o $(ODIR)/SHAW_Frozen.o $(ODIR)/SHAW_CANHUM.o $(ODIR)/Qckturf.o $(ODIR)/HERMES_SetupCrop.o $(ODIR)/CSUTS040.o $(ODIR)/SHAW_TRANSR.o $(ODIR)/SHAW_LWRMAT.o $(ODIR)/SHAW_LWRATM.o $(ODIR)/SHAW_Canlay.o $(ODIR)/SHAW_Solvrad.o $(ODIR)/SHAW_SNOWBC.o $(ODIR)/SHAW_NWSNOW.o $(ODIR)/SHAW_EBCAN.o $(ODIR)/Qckplnt.o $(ODIR)/ML_PHASEI.o $(ODIR)/SHAW_SWRSNO.o $(ODIR)/SHAW_STMMLT.o $(ODIR)/SHAW_RESGMC.o $(ODIR)/SHAW_GOSHAW.o $(ODIR)/SHAW_ADJUST.o $(ODIR)/LINDM.o $(ODIR)/Horizon.o $(ODIR)/CSREA040.o $(ODIR)/SHAW_SUMDT.o $(ODIR)/SHAW_SNOALB.o $(ODIR)/SHAW_RESHT.o $(ODIR)/SG_ROOTGR.o $(ODIR)/SG_NFACT.o $(ODIR)/ML_rootgr.o $(ODIR)/SHAW_Solar.o $(ODIR)/SHAW_RESTK.o $(ODIR)/SHAW_EBSNOW.o $(ODIR)/Rzplnt.o $(ODIR)/SHAW_STAB.o $(ODIR)/SHAW_QVSOIL.o $(ODIR)/SHAW_EBSOIL.o $(ODIR)/SHAW_Cloudy.o $(ODIR)/for_lindm.o $(ODIR)/SHAW_WBALNC.o $(ODIR)/SHAW_source.o $(ODIR)/SHAW_SOILHT.o $(ODIR)/SHAW_LWRRES.o $(ODIR)/Rzpest.o $(ODIR)/Qcktree.o $(ODIR)/ML_PHENOL.o $(ODIR)/for_sdcomp.o $(ODIR)/SHAW_WBSNOW.o $(ODIR)/SHAW_SWRCAN.o $(ODIR)/SHAW_SOILTK.o $(ODIR)/SHAW_RESVAP.o $(ODIR)/SHAW_TDMA.o $(ODIR)/SHAW_QVSNOW.o $(ODIR)/SHAW_LWRSNO.o $(ODIR)/SHAW_BACKUP.o $(ODIR)/SG_PHASEI.o $(ODIR)/KgtoKgha.o $(ODIR)/SHAW_SNOMLT.o $(ODIR)/SHAW_QVRES.o $(ODIR)/Rzrich.o $(ODIR)/Rzout.o $(ODIR)/readrzx.o $(ODIR)/Nodethick.o $(ODIR)/IPPARM.o $(ODIR)/SHAW_WBRES.o $(ODIR)/SHAW_RESHUM.o $(ODIR)/SHAW_FROST.o $(ODIR)/SDCOMP.o $(ODIR)/RZTEST.o $(ODIR)/REF_ET.o $(ODIR)/for_ipparm.o $(ODIR)/SHAW_Update.o $(ODIR)/SHAW_RTDIST.o $(ODIR)/SHAW_ENHANC.o $(ODIR)/SHAW_ATstab.o $(ODIR)/RZTEMP.o $(ODIR)/RZ-Erosion.o $(ODIR)/PT_PHASEI.o $(ODIR)/ML_NFACT.o $(ODIR)/Countdata.o $(ODIR)/Snowprms.o $(ODIR)/SHAW_SNOWTK.o $(ODIR)/SHAW_LWRCAN.o $(ODIR)/SHAW_EBRES.o $(ODIR)/NO_N2O_RATIO.o $(ODIR)/LMATCH.o $(ODIR)/HERMES_SUCROS.o $(ODIR)/ModuleDefs.o $(ODIR)/Variable.o $(ODIR)/csvlinklist.o $(ODIR)/OPSUM.o $(ODIR)/csvoutput.o $(ODIR)/WBAL.o $(ODIR)/ROOTDM.o $(ODIR)/PlantPuptake.o $(ODIR)/OpStemp.o $(ODIR)/MZ_GROSUB.o $(ODIR)/ML_GROSUB.o $(ODIR)/HMET.o $(ODIR)/for_oppest.o $(ODIR)/for_nfix.o $(ODIR)/BS_NFACTO.o $(ODIR)/ASMDM.o $(ODIR)/SG_NUPTAK.o $(ODIR)/PT_THTIME.o $(ODIR)/OPPEST.o $(ODIR)/Opgrow.o $(ODIR)/OPETPHOT.o $(ODIR)/ML_CERES.o $(ODIR)/IPPROG.o $(ODIR)/for_photo.o $(ODIR)/for_ipprog.o $(ODIR)/for_ch2oref.o $(ODIR)/BS_PHENOL.o $(ODIR)/Alt_Plant.o $(ODIR)/TotalP.o $(ODIR)/SF_OPGROW.o $(ODIR)/SF_CERES.o $(ODIR)/PODDET.o $(ODIR)/OpSoilNC.o $(ODIR)/NFIX.o $(ODIR)/for_pest.o $(ODIR)/for_ippest.o $(ODIR)/Fertilizerfate.o $(ODIR)/SF_PHENOL.o $(ODIR)/PODS.o $(ODIR)/PESTCP.o $(ODIR)/MZ_NFACTO.o $(ODIR)/IPVARC.o $(ODIR)/for_phenol.o $(ODIR)/for_mobil.o $(ODIR)/UPDATEPOOL.o $(ODIR)/SG_OPHARV.o $(ODIR)/RESPIR.o $(ODIR)/PT_OPHARV.o $(ODIR)/PEST.o $(ODIR)/Openfile.o $(ODIR)/IPSOIL.o $(ODIR)/Ipphenol.o $(ODIR)/for_pods.o $(ODIR)/for_ipphenol.o $(ODIR)/for_canopy.o $(ODIR)/CSCER040.o $(ODIR)/WEATHR.o $(ODIR)/tillage.o $(ODIR)/SEEDDM.o $(ODIR)/RStages.o $(ODIR)/OPVIEW.o $(ODIR)/MZ_OPNIT.o $(ODIR)/HeatUnit.o $(ODIR)/for_rootdm.o $(ODIR)/for_opharv.o $(ODIR)/for_freeze.o $(ODIR)/DEMAND.o $(ODIR)/CANOPY.o $(ODIR)/Breaknode.o $(ODIR)/SENES.o $(ODIR)/PlantNBal.o $(ODIR)/PET.o $(ODIR)/for_plantnbal.o $(ODIR)/for_hres_cgro.o $(ODIR)/Writeoutput.o $(ODIR)/UTILS.o $(ODIR)/Rzman.o $(ODIR)/READS.o $(ODIR)/PHOTO.o $(ODIR)/OPWBAL.o $(ODIR)/NUPTAK.o $(ODIR)/IPPEST.o $(ODIR)/for_seeddm.o $(ODIR)/for_incomp.o $(ODIR)/for_asmdm.o $(ODIR)/Cropdaycheck.o $(ODIR)/BS_GROSUB.o $(ODIR)/SPSUBS.o $(ODIR)/SoilNBal.o $(ODIR)/ROOTWU.o $(ODIR)/PT_PHENOL.o $(ODIR)/OPHARV.o $(ODIR)/MZ_CERES.o $(ODIR)/ML_opharv.o $(ODIR)/Initializepvar.o $(ODIR)/for_veggr.o $(ODIR)/for_respir.o $(ODIR)/for_opgrow.o $(ODIR)/Fertilizer.o $(ODIR)/BS_OPHARV.o $(ODIR)/BS_OPGROW.o $(ODIR)/VEGGR.o $(ODIR)/SF_OPHARV.o $(ODIR)/Rzday.o $(ODIR)/PT_ROOTGR.o $(ODIR)/ML_TILLSUB.o $(ODIR)/Labploss.o $(ODIR)/GROW.o $(ODIR)/for_roots.o $(ODIR)/DSSATDRV.o $(ODIR)/CSCERES_Interface.o $(ODIR)/Writelog.o $(ODIR)/SG_CERES.o $(ODIR)/SF_ROOTS.o $(ODIR)/PT_GROSUB.o $(ODIR)/OPWEATH.o $(ODIR)/MZ_NUPTAK.o $(ODIR)/forage.o $(ODIR)/for_grow.o $(ODIR)/Drplossrunoff.o $(ODIR)/CROPGRO.o $(ODIR)/SG_GROSUB.o $(ODIR)/SF_GROSUB.o $(ODIR)/ROOTS.o $(ODIR)/PT_SUBSTOR.o $(ODIR)/PHENOL.o $(ODIR)/MOBIL.o $(ODIR)/ML_NUPTAK.o $(ODIR)/HResCeres.o $(ODIR)/for_pestcp.o $(ODIR)/for_nuptak.o $(ODIR)/DATES.o $(ODIR)/BS_NUPTAK.o $(ODIR)/AUTHAR.o $(ODIR)/VEGDM.o $(ODIR)/PT_OPGROW.o $(ODIR)/PPlosstile.o $(ODIR)/OPGEN.o $(ODIR)/MZ_PHENOL.o $(ODIR)/IPPLNT.o $(ODIR)/for_vegdm.o $(ODIR)/for_harv.o $(ODIR)/ERROR.o $(ODIR)/BS_ROOTS.o $(ODIR)/Writeheader.o $(ODIR)/SF_OPNIT.o $(ODIR)/PBALANCE.o $(ODIR)/Manurefate.o $(ODIR)/for_rstages.o $(ODIR)/for_opmob.o $(ODIR)/for_demand.o $(ODIR)/Drplosstile.o $(ODIR)/BS_CERES.o $(ODIR)/Warning.o $(ODIR)/PPlossrunoff.o $(ODIR)/Pflux.o $(ODIR)/MZ_ROOTS.o $(ODIR)/MZ_OPGROW.o $(ODIR)/ManureP.o $(ODIR)/HRes_CGRO.o $(ODIR)/for_poddet.o $(ODIR)/ETPHR.o $(ODIR)/TRANS.o $(ODIR)/OPHEAD.o $(ODIR)/MZ_OPHARV.o $(ODIR)/INCOMP.o $(ODIR)/for_senmob.o $(ODIR)/for_ipplnt.o $(ODIR)/BS_HResCeres.o $(ODIR)/SF_NUPTAK.o $(ODIR)/SF_NFACTO.o $(ODIR)/Rznutr.o $(ODIR)/PT_NUPTAK.o $(ODIR)/OPSTRESS.o $(ODIR)/for_opview.o $(ODIR)/for_dormancy.o $(ODIR)/ETPHOT.o $(ODIR)/BS_OPNIT.o $(ODIR)/Addnode.o $(ODIR)/Rzmain.o
	$(F77_LINKER) -o $(ODIR)/RZlinux $^

# $(ODIR)/%.o: %.c 
# 	$(F77_FLAGGED) -c $@


$(ODIR)/Addnode.o: Pmodel/Addnode.for
	$(F77_FLAGGED) -c Pmodel/Addnode.for -o $@ 
$(ODIR)/Alt_Plant.o: DSSAT40/CSCER/Alt_Plant.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/Alt_Plant.for -o $@ 
$(ODIR)/ASMDM.o: DSSAT40/Generic-Pest/ASMDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/ASMDM.FOR -o $@ 
$(ODIR)/AUTHAR.o: DSSAT40/Utilities/AUTHAR.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/AUTHAR.FOR -o $@ 
$(ODIR)/Breaknode.o: Pmodel/Breaknode.for
	$(F77_FLAGGED) -c Pmodel/Breaknode.for -o $@ 
$(ODIR)/BS_CERES.o: DSSAT40/CERES-Sugarbeet/BS_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_CERES.FOR -o $@ 
$(ODIR)/BS_GROSUB.o: DSSAT40/CERES-Sugarbeet/BS_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_GROSUB.for -o $@ 
$(ODIR)/BS_HResCeres.o: DSSAT40/CERES-Sugarbeet/BS_HResCeres.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_HResCeres.for -o $@ 
$(ODIR)/BS_NFACTO.o: DSSAT40/CERES-Sugarbeet/BS_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_NFACTO.for -o $@ 
$(ODIR)/BS_NUPTAK.o: DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR -o $@ 
$(ODIR)/BS_OPGROW.o: DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR -o $@ 
$(ODIR)/BS_OPHARV.o: DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR -o $@ 
$(ODIR)/BS_OPNIT.o: DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR -o $@ 
$(ODIR)/BS_PHENOL.o: DSSAT40/CERES-Sugarbeet/BS_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_PHENOL.for -o $@ 
$(ODIR)/BS_ROOTS.o: DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR -o $@ 
$(ODIR)/CANOPY.o: DSSAT40/CROPGRO/CANOPY.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/CANOPY.FOR -o $@ 
$(ODIR)/Comp2exp.o: RZWQM/Comp2exp.for
	$(F77_FLAGGED) -c RZWQM/Comp2exp.for -o $@ 
$(ODIR)/Countdata.o: Pmodel/Countdata.for
	$(F77_FLAGGED) -c Pmodel/Countdata.for -o $@ 
$(ODIR)/Cropdaycheck.o: Pmodel/Cropdaycheck.for
	$(F77_FLAGGED) -c Pmodel/Cropdaycheck.for -o $@ 
$(ODIR)/CROPGRO.o: DSSAT40/CROPGRO/CROPGRO.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/CROPGRO.for -o $@ 
$(ODIR)/CSCER040.o: DSSAT40/CSCER/CSCER040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSCER040.FOR -o $@ 
$(ODIR)/CSCERES_Interface.o: DSSAT40/CSCER/CSCERES_Interface.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSCERES_Interface.for -o $@ 
$(ODIR)/CSREA040.o: DSSAT40/CSCER/CSREA040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSREA040.FOR -o $@ 
$(ODIR)/CSUTS040.o: DSSAT40/CSCER/CSUTS040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSUTS040.FOR -o $@ 
$(ODIR)/csvlinklist.o: DSSAT40/Utilities/CsvOuts/csvlinklist.f90
	$(F77_FLAGGED) -c DSSAT40/Utilities/CsvOuts/csvlinklist.f90 -o $@ 
$(ODIR)/csvoutput.o: DSSAT40/Utilities/CsvOuts/csvoutput.f90
	$(F77_FLAGGED) -c DSSAT40/Utilities/CsvOuts/csvoutput.f90 -o $@ 
$(ODIR)/DATES.o: DSSAT40/Utilities/DATES.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/DATES.FOR -o $@ 
$(ODIR)/DEMAND.o: DSSAT40/CROPGRO/DEMAND.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/DEMAND.FOR -o $@ 
$(ODIR)/Drplossrunoff.o: Pmodel/Drplossrunoff.for
	$(F77_FLAGGED) -c Pmodel/Drplossrunoff.for -o $@ 
$(ODIR)/Drplosstile.o: Pmodel/Drplosstile.for
	$(F77_FLAGGED) -c Pmodel/Drplosstile.for -o $@ 
$(ODIR)/DSSATDRV.o: RZWQM/DSSATDRV.for
	$(F77_FLAGGED) -c RZWQM/DSSATDRV.for -o $@ 
$(ODIR)/DSSATNFIX.o: RZWQM/DSSATNFIX.FOR
	$(F77_FLAGGED) -c RZWQM/DSSATNFIX.FOR -o $@ 
$(ODIR)/ERROR.o: DSSAT40/Utilities/ERROR.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/ERROR.for -o $@ 
$(ODIR)/ETPHOT.o: DSSAT40/CROPGRO/ETPHOT.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ETPHOT.for -o $@ 
$(ODIR)/ETPHR.o: DSSAT40/CROPGRO/ETPHR.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ETPHR.for -o $@ 
$(ODIR)/Fertilizer.o: Pmodel/Fertilizer.for
	$(F77_FLAGGED) -c Pmodel/Fertilizer.for -o $@ 
$(ODIR)/Fertilizerfate.o: Pmodel/Fertilizerfate.for
	$(F77_FLAGGED) -c Pmodel/Fertilizerfate.for -o $@ 
$(ODIR)/for_asmdm.o: DSSAT40/Forage/for_asmdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_asmdm.for -o $@ 
$(ODIR)/for_canopy.o: DSSAT40/Forage/for_canopy.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_canopy.for -o $@ 
$(ODIR)/for_ch2oref.o: DSSAT40/Forage/for_ch2oref.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ch2oref.for -o $@ 
$(ODIR)/for_demand.o: DSSAT40/Forage/for_demand.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_demand.for -o $@ 
$(ODIR)/for_dormancy.o: DSSAT40/Forage/for_dormancy.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_dormancy.for -o $@ 
$(ODIR)/for_freeze.o: DSSAT40/Forage/for_freeze.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_freeze.for -o $@ 
$(ODIR)/for_grow.o: DSSAT40/Forage/for_grow.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_grow.for -o $@ 
$(ODIR)/for_harv.o: DSSAT40/Forage/for_harv.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_harv.for -o $@ 
$(ODIR)/for_hres_cgro.o: DSSAT40/Forage/for_hres_cgro.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_hres_cgro.for -o $@ 
$(ODIR)/for_incomp.o: DSSAT40/Forage/for_incomp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_incomp.for -o $@ 
$(ODIR)/for_ipparm.o: DSSAT40/Forage/for_ipparm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipparm.for -o $@ 
$(ODIR)/for_ippest.o: DSSAT40/Forage/for_ippest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ippest.for -o $@ 
$(ODIR)/for_ipphenol.o: DSSAT40/Forage/for_ipphenol.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipphenol.for -o $@ 
$(ODIR)/for_ipplnt.o: DSSAT40/Forage/for_ipplnt.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipplnt.for -o $@ 
$(ODIR)/for_ipprog.o: DSSAT40/Forage/for_ipprog.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipprog.for -o $@ 
$(ODIR)/for_lindm.o: DSSAT40/Forage/for_lindm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_lindm.for -o $@ 
$(ODIR)/for_mobil.o: DSSAT40/Forage/for_mobil.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_mobil.for -o $@ 
$(ODIR)/for_nfix.o: DSSAT40/Forage/for_nfix.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_nfix.for -o $@ 
$(ODIR)/for_nuptak.o: DSSAT40/Forage/for_nuptak.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_nuptak.for -o $@ 
$(ODIR)/for_opgrow.o: DSSAT40/Forage/for_opgrow.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opgrow.for -o $@ 
$(ODIR)/for_opharv.o: DSSAT40/Forage/for_opharv.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opharv.for -o $@ 
$(ODIR)/for_opmob.o: DSSAT40/Forage/for_opmob.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opmob.for -o $@ 
$(ODIR)/for_oppest.o: DSSAT40/Forage/for_oppest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_oppest.for -o $@ 
$(ODIR)/for_opview.o: DSSAT40/Forage/for_opview.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opview.for -o $@ 
$(ODIR)/for_pest.o: DSSAT40/Forage/for_pest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pest.for -o $@ 
$(ODIR)/for_pestcp.o: DSSAT40/Forage/for_pestcp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pestcp.for -o $@ 
$(ODIR)/for_phenol.o: DSSAT40/Forage/for_phenol.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_phenol.for -o $@ 
$(ODIR)/for_photo.o: DSSAT40/Forage/for_photo.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_photo.for -o $@ 
$(ODIR)/for_plantnbal.o: DSSAT40/Forage/for_plantnbal.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_plantnbal.for -o $@ 
$(ODIR)/for_poddet.o: DSSAT40/Forage/for_poddet.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_poddet.for -o $@ 
$(ODIR)/for_pods.o: DSSAT40/Forage/for_pods.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pods.for -o $@ 
$(ODIR)/for_respir.o: DSSAT40/Forage/for_respir.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_respir.for -o $@ 
$(ODIR)/for_rootdm.o: DSSAT40/Forage/for_rootdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_rootdm.for -o $@ 
$(ODIR)/for_roots.o: DSSAT40/Forage/for_roots.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_roots.for -o $@ 
$(ODIR)/for_rstages.o: DSSAT40/Forage/for_rstages.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_rstages.for -o $@ 
$(ODIR)/for_sdcomp.o: DSSAT40/Forage/for_sdcomp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_sdcomp.for -o $@ 
$(ODIR)/for_seeddm.o: DSSAT40/Forage/for_seeddm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_seeddm.for -o $@ 
$(ODIR)/for_senmob.o: DSSAT40/Forage/for_senmob.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_senmob.for -o $@ 
$(ODIR)/for_vegdm.o: DSSAT40/Forage/for_vegdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_vegdm.for -o $@ 
$(ODIR)/for_veggr.o: DSSAT40/Forage/for_veggr.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_veggr.for -o $@ 
$(ODIR)/forage.o: DSSAT40/Forage/forage.for
	$(F77_FLAGGED) -c DSSAT40/Forage/forage.for -o $@ 
$(ODIR)/FREEZE.o: DSSAT40/CROPGRO/FREEZE.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/FREEZE.FOR -o $@ 
$(ODIR)/GROW.o: DSSAT40/CROPGRO/GROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/GROW.FOR -o $@ 
$(ODIR)/HeatUnit.o: Pmodel/HeatUnit.for
	$(F77_FLAGGED) -c Pmodel/HeatUnit.for -o $@ 
$(ODIR)/HERMES_Radia.o: RZWQM/HERMES_Radia.for
	$(F77_FLAGGED) -c RZWQM/HERMES_Radia.for -o $@ 
$(ODIR)/HERMES_SetupCrop.o: RZWQM/HERMES_SetupCrop.for
	$(F77_FLAGGED) -c RZWQM/HERMES_SetupCrop.for -o $@ 
$(ODIR)/HERMES_SUCROS.o: RZWQM/HERMES_SUCROS.for
	$(F77_FLAGGED) -c RZWQM/HERMES_SUCROS.for -o $@ 
$(ODIR)/HMET.o: DSSAT40/Weather/HMET.for
	$(F77_FLAGGED) -c DSSAT40/Weather/HMET.for -o $@ 
$(ODIR)/Horizon.o: RZWQM/Horizon.for
	$(F77_FLAGGED) -c RZWQM/Horizon.for -o $@ 
$(ODIR)/HRes_CGRO.o: DSSAT40/CROPGRO/HRes_CGRO.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/HRes_CGRO.for -o $@ 
$(ODIR)/HResCeres.o: DSSAT40/CSCER/HResCeres.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/HResCeres.for -o $@ 
$(ODIR)/INCOMP.o: DSSAT40/CROPGRO/INCOMP.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/INCOMP.FOR -o $@ 
$(ODIR)/Initializepvar.o: Pmodel/Initializepvar.for
	$(F77_FLAGGED) -c Pmodel/Initializepvar.for -o $@ 
$(ODIR)/IPPARM.o: DSSAT40/Generic-Pest/IPPARM.for
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPARM.for -o $@ 
$(ODIR)/IPPEST.o: DSSAT40/Generic-Pest/IPPEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPEST.FOR -o $@ 
$(ODIR)/Ipphenol.o: DSSAT40/CROPGRO/Ipphenol.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/Ipphenol.for -o $@ 
$(ODIR)/IPPLNT.o: DSSAT40/CROPGRO/IPPLNT.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/IPPLNT.FOR -o $@ 
$(ODIR)/IPPROG.o: DSSAT40/Generic-Pest/IPPROG.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPROG.FOR -o $@ 
$(ODIR)/IPSOIL.o: DSSAT40/Utilities/IPSOIL.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/IPSOIL.FOR -o $@ 
$(ODIR)/IPVARC.o: DSSAT40/Input/IPVARC.for
	$(F77_FLAGGED) -c DSSAT40/Input/IPVARC.for -o $@ 
$(ODIR)/KghaToKg.o: Pmodel/KghaToKg.for
	$(F77_FLAGGED) -c Pmodel/KghaToKg.for -o $@ 
$(ODIR)/KgtoKgha.o: Pmodel/KgtoKgha.for
	$(F77_FLAGGED) -c Pmodel/KgtoKgha.for -o $@ 
$(ODIR)/Labploss.o: Pmodel/Labploss.for
	$(F77_FLAGGED) -c Pmodel/Labploss.for -o $@ 
$(ODIR)/LINDM.o: DSSAT40/Generic-Pest/LINDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/LINDM.FOR -o $@ 
$(ODIR)/LMATCH.o: DSSAT40/Input/LMATCH.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/LMATCH.FOR -o $@ 
$(ODIR)/Manurefate.o: Pmodel/Manurefate.for
	$(F77_FLAGGED) -c Pmodel/Manurefate.for -o $@ 
$(ODIR)/ManureP.o: Pmodel/ManureP.for
	$(F77_FLAGGED) -c Pmodel/ManureP.for -o $@ 
$(ODIR)/ML_CERES.o: DSSAT40/CERES-Millet/ML_CERES.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_CERES.for -o $@ 
$(ODIR)/ML_GROSUB.o: DSSAT40/CERES-Millet/ML_GROSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_GROSUB.FOR -o $@ 
$(ODIR)/ML_NFACT.o: DSSAT40/CERES-Millet/ML_NFACT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_NFACT.FOR -o $@ 
$(ODIR)/ML_NUPTAK.o: DSSAT40/CERES-Millet/ML_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_NUPTAK.FOR -o $@ 
$(ODIR)/ML_opharv.o: DSSAT40/CERES-Millet/ML_opharv.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_opharv.for -o $@ 
$(ODIR)/ML_PHASEI.o: DSSAT40/CERES-Millet/ML_PHASEI.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_PHASEI.FOR -o $@ 
$(ODIR)/ML_PHENOL.o: DSSAT40/CERES-Millet/ML_PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_PHENOL.FOR -o $@ 
$(ODIR)/ML_rootgr.o: DSSAT40/CERES-Millet/ML_rootgr.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_rootgr.for -o $@ 
$(ODIR)/ML_TILLSUB.o: DSSAT40/CERES-Millet/ML_TILLSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_TILLSUB.FOR -o $@ 
$(ODIR)/MOBIL.o: DSSAT40/CROPGRO/MOBIL.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/MOBIL.FOR -o $@ 
$(ODIR)/ModuleDefs.o: DSSAT40/Utilities/ModuleDefs.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/ModuleDefs.for -o $@ 
$(ODIR)/MZ_CERES.o: DSSAT40/CERES-Maize/MZ_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_CERES.FOR -o $@ 
$(ODIR)/MZ_GROSUB.o: DSSAT40/CERES-Maize/MZ_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_GROSUB.for -o $@ 
$(ODIR)/MZ_NFACTO.o: DSSAT40/CERES-Maize/MZ_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_NFACTO.for -o $@ 
$(ODIR)/MZ_NUPTAK.o: DSSAT40/CERES-Maize/MZ_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_NUPTAK.FOR -o $@ 
$(ODIR)/MZ_OPGROW.o: DSSAT40/CERES-Maize/MZ_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPGROW.FOR -o $@ 
$(ODIR)/MZ_OPHARV.o: DSSAT40/CERES-Maize/MZ_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPHARV.FOR -o $@ 
$(ODIR)/MZ_OPNIT.o: DSSAT40/CERES-Maize/MZ_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPNIT.FOR -o $@ 
$(ODIR)/MZ_PHENOL.o: DSSAT40/CERES-Maize/MZ_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_PHENOL.for -o $@ 
$(ODIR)/MZ_ROOTS.o: DSSAT40/CERES-Maize/MZ_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_ROOTS.FOR -o $@ 
$(ODIR)/NFIX.o: DSSAT40/CROPGRO/NFIX.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/NFIX.FOR -o $@ 
$(ODIR)/NO_N2O_RATIO.o: RZWQM/NO_N2O_RATIO.FOR
	$(F77_FLAGGED) -c RZWQM/NO_N2O_RATIO.FOR -o $@ 
$(ODIR)/Nodethick.o: Pmodel/Nodethick.for
	$(F77_FLAGGED) -c Pmodel/Nodethick.for -o $@ 
$(ODIR)/NUPTAK.o: DSSAT40/CROPGRO/NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/NUPTAK.FOR -o $@ 
$(ODIR)/Openfile.o: Pmodel/Openfile.for
	$(F77_FLAGGED) -c Pmodel/Openfile.for -o $@ 
$(ODIR)/OPETPHOT.o: DSSAT40/CROPGRO/OPETPHOT.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/OPETPHOT.FOR -o $@ 
$(ODIR)/OPGEN.o: DSSAT40/Input/OPGEN.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/OPGEN.FOR -o $@ 
$(ODIR)/Opgrow.o: DSSAT40/CROPGRO/Opgrow.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/Opgrow.for -o $@ 
$(ODIR)/OPHARV.o: DSSAT40/CROPGRO/OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/OPHARV.FOR -o $@ 
$(ODIR)/OPHEAD.o: DSSAT40/Input/OPHEAD.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/OPHEAD.FOR -o $@ 
$(ODIR)/OPPEST.o: DSSAT40/Generic-Pest/OPPEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/OPPEST.FOR -o $@ 
$(ODIR)/OpSoilNC.o: DSSAT40/Utilities/OpSoilNC.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OpSoilNC.for -o $@ 
$(ODIR)/OpStemp.o: DSSAT40/Utilities/OpStemp.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OpStemp.for -o $@ 
$(ODIR)/OPSTRESS.o: DSSAT40/Utilities/OPSTRESS.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPSTRESS.FOR -o $@ 
$(ODIR)/OPSUM.o: DSSAT40/Utilities/OPSUM.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPSUM.for -o $@ 
$(ODIR)/OPVIEW.o: DSSAT40/Utilities/OPVIEW.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPVIEW.FOR -o $@ 
$(ODIR)/OPWBAL.o: DSSAT40/Utilities/OPWBAL.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPWBAL.for -o $@ 
$(ODIR)/OPWEATH.o: DSSAT40/Weather/OPWEATH.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/OPWEATH.FOR -o $@ 
$(ODIR)/PBALANCE.o: Pmodel/PBALANCE.for
	$(F77_FLAGGED) -c Pmodel/PBALANCE.for -o $@ 
$(ODIR)/PEST.o: DSSAT40/Generic-Pest/PEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/PEST.FOR -o $@ 
$(ODIR)/PESTCP.o: DSSAT40/Generic-Pest/PESTCP.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/PESTCP.FOR -o $@ 
$(ODIR)/PET.o: DSSAT40/Weather/PET.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/PET.FOR -o $@ 
$(ODIR)/Pflux.o: Pmodel/Pflux.for
	$(F77_FLAGGED) -c Pmodel/Pflux.for -o $@ 
$(ODIR)/PHENOL.o: DSSAT40/CROPGRO/PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PHENOL.FOR -o $@ 
$(ODIR)/PHOTO.o: DSSAT40/CROPGRO/PHOTO.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PHOTO.FOR -o $@ 
$(ODIR)/PlantNBal.o: DSSAT40/CROPGRO/PlantNBal.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PlantNBal.FOR -o $@ 
$(ODIR)/PlantPuptake.o: Pmodel/PlantPuptake.for
	$(F77_FLAGGED) -c Pmodel/PlantPuptake.for -o $@ 
$(ODIR)/PODDET.o: DSSAT40/CROPGRO/PODDET.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PODDET.FOR -o $@ 
$(ODIR)/PODS.o: DSSAT40/CROPGRO/PODS.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PODS.FOR -o $@ 
$(ODIR)/PPlossrunoff.o: Pmodel/PPlossrunoff.for
	$(F77_FLAGGED) -c Pmodel/PPlossrunoff.for -o $@ 
$(ODIR)/PPlosstile.o: Pmodel/PPlosstile.for
	$(F77_FLAGGED) -c Pmodel/PPlosstile.for -o $@ 
$(ODIR)/PT_GROSUB.o: DSSAT40/SUBSTOR-Potato/PT_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_GROSUB.for -o $@ 
$(ODIR)/PT_NFACTO.o: DSSAT40/SUBSTOR-Potato/PT_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_NFACTO.for -o $@ 
$(ODIR)/PT_NUPTAK.o: DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for -o $@ 
$(ODIR)/PT_OPGROW.o: DSSAT40/SUBSTOR-Potato/PT_OPGROW.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_OPGROW.for -o $@ 
$(ODIR)/PT_OPHARV.o: DSSAT40/SUBSTOR-Potato/PT_OPHARV.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_OPHARV.for -o $@ 
$(ODIR)/PT_PHASEI.o: DSSAT40/SUBSTOR-Potato/PT_PHASEI.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_PHASEI.for -o $@ 
$(ODIR)/PT_PHENOL.o: DSSAT40/SUBSTOR-Potato/PT_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_PHENOL.for -o $@ 
$(ODIR)/PT_ROOTGR.o: DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for -o $@ 
$(ODIR)/PT_SUBSTOR.o: DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR -o $@ 
$(ODIR)/PT_THTIME.o: DSSAT40/SUBSTOR-Potato/PT_THTIME.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_THTIME.for -o $@ 
$(ODIR)/Qckplnt.o: RZWQM/Qckplnt.for
	$(F77_FLAGGED) -c RZWQM/Qckplnt.for -o $@ 
$(ODIR)/Qcktree.o: RZWQM/Qcktree.for
	$(F77_FLAGGED) -c RZWQM/Qcktree.for -o $@ 
$(ODIR)/Qckturf.o: RZWQM/Qckturf.for
	$(F77_FLAGGED) -c RZWQM/Qckturf.for -o $@ 
$(ODIR)/readrzx.o: RZWQM/readrzx.for
	$(F77_FLAGGED) -c RZWQM/readrzx.for -o $@ 
$(ODIR)/READS.o: DSSAT40/Utilities/READS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/READS.for -o $@ 
$(ODIR)/REF_ET.o: RZWQM/REF_ET.FOR
	$(F77_FLAGGED) -c RZWQM/REF_ET.FOR -o $@ 
$(ODIR)/RESPIR.o: DSSAT40/CROPGRO/RESPIR.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/RESPIR.FOR -o $@ 
$(ODIR)/ROOTDM.o: DSSAT40/Generic-Pest/ROOTDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/ROOTDM.FOR -o $@ 
$(ODIR)/ROOTS.o: DSSAT40/CROPGRO/ROOTS.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ROOTS.for -o $@ 
$(ODIR)/ROOTWU.o: DSSAT40/Utilities/ROOTWU.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/ROOTWU.FOR -o $@ 
$(ODIR)/RStages.o: DSSAT40/CROPGRO/RStages.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/RStages.for -o $@ 
$(ODIR)/RZ-Erosion.o: GLEAMS/RZ-Erosion.for
	$(F77_FLAGGED) -c GLEAMS/RZ-Erosion.for -o $@ 
$(ODIR)/Rzchem.o: RZWQM/Rzchem.for
	$(F77_FLAGGED) -c RZWQM/Rzchem.for -o $@ 
$(ODIR)/Rzday.o: RZWQM/Rzday.for
	$(F77_FLAGGED) -c RZWQM/Rzday.for -o $@ 
$(ODIR)/Rzmain.o: RZWQM/Rzmain.for
	$(F77_FLAGGED) -c RZWQM/Rzmain.for -o $@ 
$(ODIR)/Rzman.o: RZWQM/Rzman.for
	$(F77_FLAGGED) -c RZWQM/Rzman.for -o $@ 
$(ODIR)/Rznutr.o: RZWQM/Rznutr.for
	$(F77_FLAGGED) -c RZWQM/Rznutr.for -o $@ 
$(ODIR)/Rzout.o: RZWQM/Rzout.for
	$(F77_FLAGGED) -c RZWQM/Rzout.for -o $@ 
$(ODIR)/Rzpest.o: RZWQM/Rzpest.for
	$(F77_FLAGGED) -c RZWQM/Rzpest.for -o $@ 
$(ODIR)/Rzpet.o: RZWQM/Rzpet.for
	$(F77_FLAGGED) -c RZWQM/Rzpet.for -o $@ 
$(ODIR)/Rzplnt.o: RZWQM/Rzplnt.for
	$(F77_FLAGGED) -c RZWQM/Rzplnt.for -o $@ 
$(ODIR)/Rzrich.o: RZWQM/Rzrich.for
	$(F77_FLAGGED) -c RZWQM/Rzrich.for -o $@ 
$(ODIR)/RZTEMP.o: RZWQM/RZTEMP.FOR
	$(F77_FLAGGED) -c RZWQM/RZTEMP.FOR -o $@ 
$(ODIR)/RZTEST.o: RZWQM/RZTEST.for
	$(F77_FLAGGED) -c RZWQM/RZTEST.for -o $@ 
$(ODIR)/SDCOMP.o: DSSAT40/CROPGRO/SDCOMP.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/SDCOMP.FOR -o $@ 
$(ODIR)/SEEDDM.o: DSSAT40/Generic-Pest/SEEDDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/SEEDDM.FOR -o $@ 
$(ODIR)/SENES.o: DSSAT40/CROPGRO/SENES.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/SENES.FOR -o $@ 
$(ODIR)/SF_CERES.o: DSSAT40/CERES-Sunflower/SF_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_CERES.FOR -o $@ 
$(ODIR)/SF_GROSUB.o: DSSAT40/CERES-Sunflower/SF_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_GROSUB.for -o $@ 
$(ODIR)/SF_NFACTO.o: DSSAT40/CERES-Sunflower/SF_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_NFACTO.for -o $@ 
$(ODIR)/SF_NUPTAK.o: DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR -o $@ 
$(ODIR)/SF_OPGROW.o: DSSAT40/CERES-Sunflower/SF_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPGROW.FOR -o $@ 
$(ODIR)/SF_OPHARV.o: DSSAT40/CERES-Sunflower/SF_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPHARV.FOR -o $@ 
$(ODIR)/SF_OPNIT.o: DSSAT40/CERES-Sunflower/SF_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPNIT.FOR -o $@ 
$(ODIR)/SF_PHENOL.o: DSSAT40/CERES-Sunflower/SF_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_PHENOL.for -o $@ 
$(ODIR)/SF_ROOTS.o: DSSAT40/CERES-Sunflower/SF_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_ROOTS.FOR -o $@ 
$(ODIR)/SG_CERES.o: DSSAT40/CERES-Sorghum/SG_CERES.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_CERES.for -o $@ 
$(ODIR)/SG_GROSUB.o: DSSAT40/CERES-Sorghum/SG_GROSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_GROSUB.FOR -o $@ 
$(ODIR)/SG_NFACT.o: DSSAT40/CERES-Sorghum/SG_NFACT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_NFACT.FOR -o $@ 
$(ODIR)/SG_NUPTAK.o: DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR -o $@ 
$(ODIR)/SG_OPHARV.o: DSSAT40/CERES-Sorghum/SG_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_OPHARV.FOR -o $@ 
$(ODIR)/SG_PHASEI.o: DSSAT40/CERES-Sorghum/SG_PHASEI.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_PHASEI.FOR -o $@ 
$(ODIR)/SG_PHENOL.o: DSSAT40/CERES-Sorghum/SG_PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_PHENOL.FOR -o $@ 
$(ODIR)/SG_ROOTGR.o: DSSAT40/CERES-Sorghum/SG_ROOTGR.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_ROOTGR.for -o $@ 
$(ODIR)/SHAW_ADJUST.o: SHAW24b/SHAW_ADJUST.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_ADJUST.FOR -o $@ 
$(ODIR)/SHAW_ATstab.o: SHAW24b/SHAW_ATstab.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_ATstab.for -o $@ 
$(ODIR)/SHAW_BACKUP.o: SHAW24b/SHAW_BACKUP.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_BACKUP.FOR -o $@ 
$(ODIR)/SHAW_CANHUM.o: SHAW24b/SHAW_CANHUM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_CANHUM.FOR -o $@ 
$(ODIR)/SHAW_Canlay.o: SHAW24b/SHAW_Canlay.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Canlay.for -o $@ 
$(ODIR)/SHAW_CANTK.o: SHAW24b/SHAW_CANTK.For
	$(F77_FLAGGED) -c SHAW24b/SHAW_CANTK.For -o $@ 
$(ODIR)/SHAW_Cloudy.o: SHAW24b/SHAW_Cloudy.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Cloudy.for -o $@ 
$(ODIR)/SHAW_Conduc.o: SHAW24b/SHAW_Conduc.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Conduc.for -o $@ 
$(ODIR)/SHAW_EBCAN.o: SHAW24b/SHAW_EBCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBCAN.FOR -o $@ 
$(ODIR)/SHAW_EBRES.o: SHAW24b/SHAW_EBRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBRES.FOR -o $@ 
$(ODIR)/SHAW_EBSNOW.o: SHAW24b/SHAW_EBSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBSNOW.FOR -o $@ 
$(ODIR)/SHAW_EBSOIL.o: SHAW24b/SHAW_EBSOIL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBSOIL.FOR -o $@ 
$(ODIR)/SHAW_ENHANC.o: SHAW24b/SHAW_ENHANC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_ENHANC.FOR -o $@ 
$(ODIR)/SHAW_FROST.o: SHAW24b/SHAW_FROST.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_FROST.FOR -o $@ 
$(ODIR)/SHAW_Frozen.o: SHAW24b/SHAW_Frozen.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Frozen.for -o $@ 
$(ODIR)/SHAW_FSLOPE.o: SHAW24b/SHAW_FSLOPE.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_FSLOPE.FOR -o $@ 
$(ODIR)/SHAW_GOSHAW.o: SHAW24b/SHAW_GOSHAW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_GOSHAW.FOR -o $@ 
$(ODIR)/SHAW_LEAFT.o: SHAW24b/SHAW_LEAFT.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_LEAFT.for -o $@ 
$(ODIR)/SHAW_LWRATM.o: SHAW24b/SHAW_LWRATM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRATM.FOR -o $@ 
$(ODIR)/SHAW_LWRBAL.o: SHAW24b/SHAW_LWRBAL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRBAL.FOR -o $@ 
$(ODIR)/SHAW_LWRCAN.o: SHAW24b/SHAW_LWRCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRCAN.FOR -o $@ 
$(ODIR)/SHAW_LWRMAT.o: SHAW24b/SHAW_LWRMAT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRMAT.FOR -o $@ 
$(ODIR)/SHAW_LWRRES.o: SHAW24b/SHAW_LWRRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRRES.FOR -o $@ 
$(ODIR)/SHAW_LWRSNO.o: SHAW24b/SHAW_LWRSNO.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRSNO.FOR -o $@ 
$(ODIR)/SHAW_META.o: SHAW24b/SHAW_META.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_META.FOR -o $@ 
$(ODIR)/SHAW_NWSNOW.o: SHAW24b/SHAW_NWSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_NWSNOW.FOR -o $@ 
$(ODIR)/SHAW_Photosyn.o: SHAW24b/SHAW_Photosyn.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Photosyn.for -o $@ 
$(ODIR)/SHAW_QVRES.o: SHAW24b/SHAW_QVRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVRES.FOR -o $@ 
$(ODIR)/SHAW_QVSNOW.o: SHAW24b/SHAW_QVSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVSNOW.FOR -o $@ 
$(ODIR)/SHAW_QVSOIL.o: SHAW24b/SHAW_QVSOIL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVSOIL.FOR -o $@ 
$(ODIR)/SHAW_RESGMC.o: SHAW24b/SHAW_RESGMC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESGMC.FOR -o $@ 
$(ODIR)/SHAW_RESHT.o: SHAW24b/SHAW_RESHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESHT.FOR -o $@ 
$(ODIR)/SHAW_RESHUM.o: SHAW24b/SHAW_RESHUM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESHUM.FOR -o $@ 
$(ODIR)/SHAW_RESTK.o: SHAW24b/SHAW_RESTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESTK.FOR -o $@ 
$(ODIR)/SHAW_RESVAP.o: SHAW24b/SHAW_RESVAP.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESVAP.FOR -o $@ 
$(ODIR)/SHAW_RESVK.o: SHAW24b/SHAW_RESVK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESVK.FOR -o $@ 
$(ODIR)/SHAW_RTDIST.o: SHAW24b/SHAW_RTDIST.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_RTDIST.for -o $@ 
$(ODIR)/SHAW_SNOALB.o: SHAW24b/SHAW_SNOALB.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOALB.FOR -o $@ 
$(ODIR)/SHAW_SNOMLT.o: SHAW24b/SHAW_SNOMLT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOMLT.FOR -o $@ 
$(ODIR)/SHAW_SNOWBC.o: SHAW24b/SHAW_SNOWBC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWBC.FOR -o $@ 
$(ODIR)/SHAW_SNOWHT.o: SHAW24b/SHAW_SNOWHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWHT.FOR -o $@ 
$(ODIR)/SHAW_SNOWTK.o: SHAW24b/SHAW_SNOWTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWTK.FOR -o $@ 
$(ODIR)/SHAW_SOILHT.o: SHAW24b/SHAW_SOILHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILHT.FOR -o $@ 
$(ODIR)/SHAW_SOILR.o: SHAW24b/SHAW_SOILR.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILR.FOR -o $@ 
$(ODIR)/SHAW_SOILTK.o: SHAW24b/SHAW_SOILTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILTK.FOR -o $@ 
$(ODIR)/SHAW_Solar.o: SHAW24b/SHAW_Solar.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Solar.for -o $@ 
$(ODIR)/SHAW_Solvrad.o: SHAW24b/SHAW_Solvrad.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Solvrad.for -o $@ 
$(ODIR)/SHAW_source.o: SHAW24b/SHAW_source.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_source.for -o $@ 
$(ODIR)/SHAW_STAB.o: SHAW24b/SHAW_STAB.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_STAB.FOR -o $@ 
$(ODIR)/SHAW_STMMLT.o: SHAW24b/SHAW_STMMLT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_STMMLT.FOR -o $@ 
$(ODIR)/SHAW_SUMDT.o: SHAW24b/SHAW_SUMDT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SUMDT.FOR -o $@ 
$(ODIR)/SHAW_SWRBAL.o: SHAW24b/SHAW_SWRBAL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRBAL.FOR -o $@ 
$(ODIR)/SHAW_SWRCAN.o: SHAW24b/SHAW_SWRCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRCAN.FOR -o $@ 
$(ODIR)/SHAW_SWRSNO.o: SHAW24b/SHAW_SWRSNO.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRSNO.FOR -o $@ 
$(ODIR)/SHAW_TDMA.o: SHAW24b/SHAW_TDMA.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TDMA.FOR -o $@ 
$(ODIR)/SHAW_TRANSC.o: SHAW24b/SHAW_TRANSC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TRANSC.FOR -o $@ 
$(ODIR)/SHAW_TRANSR.o: SHAW24b/SHAW_TRANSR.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TRANSR.FOR -o $@ 
$(ODIR)/SHAW_Update.o: SHAW24b/SHAW_Update.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Update.for -o $@ 
$(ODIR)/SHAW_VSLOPE.o: SHAW24b/SHAW_VSLOPE.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_VSLOPE.FOR -o $@ 
$(ODIR)/SHAW_WBALNC.o: SHAW24b/SHAW_WBALNC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBALNC.FOR -o $@ 
$(ODIR)/SHAW_WBCAN.o: SHAW24b/SHAW_WBCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBCAN.FOR -o $@ 
$(ODIR)/SHAW_WBRES.o: SHAW24b/SHAW_WBRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBRES.FOR -o $@ 
$(ODIR)/SHAW_WBSNOW.o: SHAW24b/SHAW_WBSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBSNOW.FOR -o $@ 
$(ODIR)/SHAW_Weight.o: SHAW24b/SHAW_Weight.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Weight.for -o $@ 
$(ODIR)/Snowprms.o: RZWQM/Snowprms.for
	$(F77_FLAGGED) -c RZWQM/Snowprms.for -o $@ 
$(ODIR)/SoilNBal.o: DSSAT40/Utilities/SoilNBal.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/SoilNBal.FOR -o $@ 
$(ODIR)/SOLAR.o: DSSAT40/Weather/SOLAR.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/SOLAR.FOR -o $@ 
$(ODIR)/SPSUBS.o: DSSAT40/Utilities/SPSUBS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/SPSUBS.for -o $@ 
$(ODIR)/tillage.o: Pmodel/tillage.for
	$(F77_FLAGGED) -c Pmodel/tillage.for -o $@ 
$(ODIR)/TotalP.o: Pmodel/TotalP.for
	$(F77_FLAGGED) -c Pmodel/TotalP.for -o $@ 
$(ODIR)/TRANS.o: DSSAT40/Utilities/TRANS.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/TRANS.FOR -o $@ 
$(ODIR)/UPDATEPOOL.o: Pmodel/UPDATEPOOL.for
	$(F77_FLAGGED) -c Pmodel/UPDATEPOOL.for -o $@ 
$(ODIR)/UTILS.o: DSSAT40/Utilities/UTILS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/UTILS.for -o $@ 
$(ODIR)/Variable.o: Pmodel/Variable.for
	$(F77_FLAGGED) -c Pmodel/Variable.for -o $@ 
$(ODIR)/VEGDM.o: DSSAT40/Generic-Pest/VEGDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/VEGDM.FOR -o $@ 
$(ODIR)/VEGGR.o: DSSAT40/CROPGRO/VEGGR.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/VEGGR.FOR -o $@ 
$(ODIR)/Warning.o: DSSAT40/Utilities/Warning.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/Warning.FOR -o $@ 
$(ODIR)/WBAL.o: DSSAT40/Utilities/WBAL.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/WBAL.for -o $@ 
$(ODIR)/WEATHR.o: DSSAT40/Weather/WEATHR.for
	$(F77_FLAGGED) -c DSSAT40/Weather/WEATHR.for -o $@ 
$(ODIR)/Writeheader.o: Pmodel/Writeheader.for
	$(F77_FLAGGED) -c Pmodel/Writeheader.for -o $@ 
$(ODIR)/Writelog.o: Pmodel/Writelog.for
	$(F77_FLAGGED) -c Pmodel/Writelog.for -o $@ 
$(ODIR)/Writeoutput.o: Pmodel/Writeoutput.for
	$(F77_FLAGGED) -c Pmodel/Writeoutput.for -o $@

.PHONY: clean
clean: 
	rm -f $(ODIR)/*.o $(ODIR)/*.mod
