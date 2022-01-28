F77=ifort
ODIR = Release
F77_FLAGGED=$(F77) -nologo -fpp -shared-intel -save -zero -module $(ODIR)/ -Fo$(ODIR)/ -I$(ODIR)/ -module $(ODIR)/ -static -threads -f77rtl

Release/Rzmain: Release/SHAW_WBCAN.o Release/SHAW_Photosyn.o Release/SHAW_LWRBAL.o Release/SG_PHENOL.o Release/Rzpet.o Release/DSSATNFIX.o Release/SHAW_VSLOPE.o Release/SHAW_TRANSC.o Release/SHAW_RESVK.o Release/SHAW_META.o Release/PT_NFACTO.o Release/HERMES_Radia.o Release/SHAW_Weight.o Release/SHAW_SOILR.o Release/SHAW_SNOWHT.o Release/SHAW_LEAFT.o Release/SHAW_Conduc.o Release/Rzchem.o Release/KghaToKg.o Release/FREEZE.o Release/Comp2exp.o Release/SOLAR.o Release/SHAW_SWRBAL.o Release/SHAW_FSLOPE.o Release/SHAW_Frozen.o Release/SHAW_CANHUM.o Release/Qckturf.o Release/HERMES_SetupCrop.o Release/CSUTS040.o Release/SHAW_TRANSR.o Release/SHAW_LWRMAT.o Release/SHAW_LWRATM.o Release/SHAW_Canlay.o Release/SHAW_Solvrad.o Release/SHAW_SNOWBC.o Release/SHAW_NWSNOW.o Release/SHAW_EBCAN.o Release/Qckplnt.o Release/ML_PHASEI.o Release/SHAW_SWRSNO.o Release/SHAW_STMMLT.o Release/SHAW_RESGMC.o Release/SHAW_GOSHAW.o Release/SHAW_ADJUST.o Release/LINDM.o Release/Horizon.o Release/CSREA040.o Release/SHAW_SUMDT.o Release/SHAW_SNOALB.o Release/SHAW_RESHT.o Release/SG_ROOTGR.o Release/SG_NFACT.o Release/ML_rootgr.o Release/SHAW_Solar.o Release/SHAW_RESTK.o Release/SHAW_EBSNOW.o Release/Rzplnt.o Release/SHAW_STAB.o Release/SHAW_QVSOIL.o Release/SHAW_EBSOIL.o Release/SHAW_Cloudy.o Release/for_lindm.o Release/SHAW_WBALNC.o Release/SHAW_source.o Release/SHAW_SOILHT.o Release/SHAW_LWRRES.o Release/Rzpest.o Release/Qcktree.o Release/ML_PHENOL.o Release/for_sdcomp.o Release/SHAW_WBSNOW.o Release/SHAW_SWRCAN.o Release/SHAW_SOILTK.o Release/SHAW_RESVAP.o Release/SHAW_TDMA.o Release/SHAW_QVSNOW.o Release/SHAW_LWRSNO.o Release/SHAW_BACKUP.o Release/SG_PHASEI.o Release/KgtoKgha.o Release/SHAW_SNOMLT.o Release/SHAW_QVRES.o Release/Rzrich.o Release/Rzout.o Release/readrzx.o Release/Nodethick.o Release/IPPARM.o Release/SHAW_WBRES.o Release/SHAW_RESHUM.o Release/SHAW_FROST.o Release/SDCOMP.o Release/RZTEST.o Release/REF_ET.o Release/for_ipparm.o Release/SHAW_Update.o Release/SHAW_RTDIST.o Release/SHAW_ENHANC.o Release/SHAW_ATstab.o Release/RZTEMP.o Release/RZ-Erosion.o Release/PT_PHASEI.o Release/ML_NFACT.o Release/Countdata.o Release/Snowprms.o Release/SHAW_SNOWTK.o Release/SHAW_LWRCAN.o Release/SHAW_EBRES.o Release/NO_N2O_RATIO.o Release/LMATCH.o Release/HERMES_SUCROS.o Release/ModuleDefs.o Release/Variable.o Release/csvlinklist.o Release/OPSUM.o Release/csvoutput.o Release/WBAL.o Release/ROOTDM.o Release/PlantPuptake.o Release/OpStemp.o Release/MZ_GROSUB.o Release/ML_GROSUB.o Release/HMET.o Release/for_oppest.o Release/for_nfix.o Release/BS_NFACTO.o Release/ASMDM.o Release/SG_NUPTAK.o Release/PT_THTIME.o Release/OPPEST.o Release/Opgrow.o Release/OPETPHOT.o Release/ML_CERES.o Release/IPPROG.o Release/for_photo.o Release/for_ipprog.o Release/for_ch2oref.o Release/BS_PHENOL.o Release/Alt_Plant.o Release/TotalP.o Release/SF_OPGROW.o Release/SF_CERES.o Release/PODDET.o Release/OpSoilNC.o Release/NFIX.o Release/for_pest.o Release/for_ippest.o Release/Fertilizerfate.o Release/SF_PHENOL.o Release/PODS.o Release/PESTCP.o Release/MZ_NFACTO.o Release/IPVARC.o Release/for_phenol.o Release/for_mobil.o Release/UPDATEPOOL.o Release/SG_OPHARV.o Release/RESPIR.o Release/PT_OPHARV.o Release/PEST.o Release/Openfile.o Release/IPSOIL.o Release/Ipphenol.o Release/for_pods.o Release/for_ipphenol.o Release/for_canopy.o Release/CSCER040.o Release/WEATHR.o Release/tillage.o Release/SEEDDM.o Release/RStages.o Release/OPVIEW.o Release/MZ_OPNIT.o Release/HeatUnit.o Release/for_rootdm.o Release/for_opharv.o Release/for_freeze.o Release/DEMAND.o Release/CANOPY.o Release/Breaknode.o Release/SENES.o Release/Rzmain.o Release/PlantNBal.o Release/PET.o Release/for_plantnbal.o Release/for_hres_cgro.o Release/Writeoutput.o Release/UTILS.o Release/Rzman.o Release/READS.o Release/PHOTO.o Release/OPWBAL.o Release/NUPTAK.o Release/IPPEST.o Release/for_seeddm.o Release/for_incomp.o Release/for_asmdm.o Release/Cropdaycheck.o Release/BS_GROSUB.o Release/SPSUBS.o Release/SoilNBal.o Release/ROOTWU.o Release/PT_PHENOL.o Release/OPHARV.o Release/MZ_CERES.o Release/ML_opharv.o Release/Initializepvar.o Release/for_veggr.o Release/for_respir.o Release/for_opgrow.o Release/Fertilizer.o Release/BS_OPHARV.o Release/BS_OPGROW.o Release/VEGGR.o Release/SF_OPHARV.o Release/Rzday.o Release/PT_ROOTGR.o Release/ML_TILLSUB.o Release/Labploss.o Release/GROW.o Release/for_roots.o Release/DSSATDRV.o Release/CSCERES_Interface.o Release/Writelog.o Release/SG_CERES.o Release/SF_ROOTS.o Release/PT_GROSUB.o Release/OPWEATH.o Release/MZ_NUPTAK.o Release/forage.o Release/for_grow.o Release/Drplossrunoff.o Release/CROPGRO.o Release/SG_GROSUB.o Release/SF_GROSUB.o Release/ROOTS.o Release/PT_SUBSTOR.o Release/PHENOL.o Release/MOBIL.o Release/ML_NUPTAK.o Release/HResCeres.o Release/for_pestcp.o Release/for_nuptak.o Release/DATES.o Release/BS_NUPTAK.o Release/AUTHAR.o Release/VEGDM.o Release/PT_OPGROW.o Release/PPlosstile.o Release/OPGEN.o Release/MZ_PHENOL.o Release/IPPLNT.o Release/for_vegdm.o Release/for_harv.o Release/ERROR.o Release/BS_ROOTS.o Release/Writeheader.o Release/SF_OPNIT.o Release/PBALANCE.o Release/Manurefate.o Release/for_rstages.o Release/for_opmob.o Release/for_demand.o Release/Drplosstile.o Release/BS_CERES.o Release/Warning.o Release/PPlossrunoff.o Release/Pflux.o Release/MZ_ROOTS.o Release/MZ_OPGROW.o Release/ManureP.o Release/HRes_CGRO.o Release/for_poddet.o Release/ETPHR.o Release/TRANS.o Release/OPHEAD.o Release/MZ_OPHARV.o Release/INCOMP.o Release/for_senmob.o Release/for_ipplnt.o Release/BS_HResCeres.o Release/SF_NUPTAK.o Release/SF_NFACTO.o Release/Rznutr.o Release/PT_NUPTAK.o Release/OPSTRESS.o Release/for_opview.o Release/for_dormancy.o Release/ETPHOT.o Release/BS_OPNIT.o Release/Addnode.o
	$(F77) -o Release/RZlinux -threads -static Release/SHAW_WBCAN.o Release/SHAW_Photosyn.o Release/SHAW_LWRBAL.o Release/SG_PHENOL.o Release/Rzpet.o Release/DSSATNFIX.o Release/SHAW_VSLOPE.o Release/SHAW_TRANSC.o Release/SHAW_RESVK.o Release/SHAW_META.o Release/PT_NFACTO.o Release/HERMES_Radia.o Release/SHAW_Weight.o Release/SHAW_SOILR.o Release/SHAW_SNOWHT.o Release/SHAW_LEAFT.o Release/SHAW_Conduc.o Release/Rzchem.o Release/KghaToKg.o Release/FREEZE.o Release/Comp2exp.o Release/SOLAR.o Release/SHAW_SWRBAL.o Release/SHAW_FSLOPE.o Release/SHAW_Frozen.o Release/SHAW_CANHUM.o Release/Qckturf.o Release/HERMES_SetupCrop.o Release/CSUTS040.o Release/SHAW_TRANSR.o Release/SHAW_LWRMAT.o Release/SHAW_LWRATM.o Release/SHAW_Canlay.o Release/SHAW_Solvrad.o Release/SHAW_SNOWBC.o Release/SHAW_NWSNOW.o Release/SHAW_EBCAN.o Release/Qckplnt.o Release/ML_PHASEI.o Release/SHAW_SWRSNO.o Release/SHAW_STMMLT.o Release/SHAW_RESGMC.o Release/SHAW_GOSHAW.o Release/SHAW_ADJUST.o Release/LINDM.o Release/Horizon.o Release/CSREA040.o Release/SHAW_SUMDT.o Release/SHAW_SNOALB.o Release/SHAW_RESHT.o Release/SG_ROOTGR.o Release/SG_NFACT.o Release/ML_rootgr.o Release/SHAW_Solar.o Release/SHAW_RESTK.o Release/SHAW_EBSNOW.o Release/Rzplnt.o Release/SHAW_STAB.o Release/SHAW_QVSOIL.o Release/SHAW_EBSOIL.o Release/SHAW_Cloudy.o Release/for_lindm.o Release/SHAW_WBALNC.o Release/SHAW_source.o Release/SHAW_SOILHT.o Release/SHAW_LWRRES.o Release/Rzpest.o Release/Qcktree.o Release/ML_PHENOL.o Release/for_sdcomp.o Release/SHAW_WBSNOW.o Release/SHAW_SWRCAN.o Release/SHAW_SOILTK.o Release/SHAW_RESVAP.o Release/SHAW_TDMA.o Release/SHAW_QVSNOW.o Release/SHAW_LWRSNO.o Release/SHAW_BACKUP.o Release/SG_PHASEI.o Release/KgtoKgha.o Release/SHAW_SNOMLT.o Release/SHAW_QVRES.o Release/Rzrich.o Release/Rzout.o Release/readrzx.o Release/Nodethick.o Release/IPPARM.o Release/SHAW_WBRES.o Release/SHAW_RESHUM.o Release/SHAW_FROST.o Release/SDCOMP.o Release/RZTEST.o Release/REF_ET.o Release/for_ipparm.o Release/SHAW_Update.o Release/SHAW_RTDIST.o Release/SHAW_ENHANC.o Release/SHAW_ATstab.o Release/RZTEMP.o Release/RZ-Erosion.o Release/PT_PHASEI.o Release/ML_NFACT.o Release/Countdata.o Release/Snowprms.o Release/SHAW_SNOWTK.o Release/SHAW_LWRCAN.o Release/SHAW_EBRES.o Release/NO_N2O_RATIO.o Release/LMATCH.o Release/HERMES_SUCROS.o Release/ModuleDefs.o Release/Variable.o Release/csvlinklist.o Release/OPSUM.o Release/csvoutput.o Release/WBAL.o Release/ROOTDM.o Release/PlantPuptake.o Release/OpStemp.o Release/MZ_GROSUB.o Release/ML_GROSUB.o Release/HMET.o Release/for_oppest.o Release/for_nfix.o Release/BS_NFACTO.o Release/ASMDM.o Release/SG_NUPTAK.o Release/PT_THTIME.o Release/OPPEST.o Release/Opgrow.o Release/OPETPHOT.o Release/ML_CERES.o Release/IPPROG.o Release/for_photo.o Release/for_ipprog.o Release/for_ch2oref.o Release/BS_PHENOL.o Release/Alt_Plant.o Release/TotalP.o Release/SF_OPGROW.o Release/SF_CERES.o Release/PODDET.o Release/OpSoilNC.o Release/NFIX.o Release/for_pest.o Release/for_ippest.o Release/Fertilizerfate.o Release/SF_PHENOL.o Release/PODS.o Release/PESTCP.o Release/MZ_NFACTO.o Release/IPVARC.o Release/for_phenol.o Release/for_mobil.o Release/UPDATEPOOL.o Release/SG_OPHARV.o Release/RESPIR.o Release/PT_OPHARV.o Release/PEST.o Release/Openfile.o Release/IPSOIL.o Release/Ipphenol.o Release/for_pods.o Release/for_ipphenol.o Release/for_canopy.o Release/CSCER040.o Release/WEATHR.o Release/tillage.o Release/SEEDDM.o Release/RStages.o Release/OPVIEW.o Release/MZ_OPNIT.o Release/HeatUnit.o Release/for_rootdm.o Release/for_opharv.o Release/for_freeze.o Release/DEMAND.o Release/CANOPY.o Release/Breaknode.o Release/SENES.o Release/Rzmain.o Release/PlantNBal.o Release/PET.o Release/for_plantnbal.o Release/for_hres_cgro.o Release/Writeoutput.o Release/UTILS.o Release/Rzman.o Release/READS.o Release/PHOTO.o Release/OPWBAL.o Release/NUPTAK.o Release/IPPEST.o Release/for_seeddm.o Release/for_incomp.o Release/for_asmdm.o Release/Cropdaycheck.o Release/BS_GROSUB.o Release/SPSUBS.o Release/SoilNBal.o Release/ROOTWU.o Release/PT_PHENOL.o Release/OPHARV.o Release/MZ_CERES.o Release/ML_opharv.o Release/Initializepvar.o Release/for_veggr.o Release/for_respir.o Release/for_opgrow.o Release/Fertilizer.o Release/BS_OPHARV.o Release/BS_OPGROW.o Release/VEGGR.o Release/SF_OPHARV.o Release/Rzday.o Release/PT_ROOTGR.o Release/ML_TILLSUB.o Release/Labploss.o Release/GROW.o Release/for_roots.o Release/DSSATDRV.o Release/CSCERES_Interface.o Release/Writelog.o Release/SG_CERES.o Release/SF_ROOTS.o Release/PT_GROSUB.o Release/OPWEATH.o Release/MZ_NUPTAK.o Release/forage.o Release/for_grow.o Release/Drplossrunoff.o Release/CROPGRO.o Release/SG_GROSUB.o Release/SF_GROSUB.o Release/ROOTS.o Release/PT_SUBSTOR.o Release/PHENOL.o Release/MOBIL.o Release/ML_NUPTAK.o Release/HResCeres.o Release/for_pestcp.o Release/for_nuptak.o Release/DATES.o Release/BS_NUPTAK.o Release/AUTHAR.o Release/VEGDM.o Release/PT_OPGROW.o Release/PPlosstile.o Release/OPGEN.o Release/MZ_PHENOL.o Release/IPPLNT.o Release/for_vegdm.o Release/for_harv.o Release/ERROR.o Release/BS_ROOTS.o Release/Writeheader.o Release/SF_OPNIT.o Release/PBALANCE.o Release/Manurefate.o Release/for_rstages.o Release/for_opmob.o Release/for_demand.o Release/Drplosstile.o Release/BS_CERES.o Release/Warning.o Release/PPlossrunoff.o Release/Pflux.o Release/MZ_ROOTS.o Release/MZ_OPGROW.o Release/ManureP.o Release/HRes_CGRO.o Release/for_poddet.o Release/ETPHR.o Release/TRANS.o Release/OPHEAD.o Release/MZ_OPHARV.o Release/INCOMP.o Release/for_senmob.o Release/for_ipplnt.o Release/BS_HResCeres.o Release/SF_NUPTAK.o Release/SF_NFACTO.o Release/Rznutr.o Release/PT_NUPTAK.o Release/OPSTRESS.o Release/for_opview.o Release/for_dormancy.o Release/ETPHOT.o Release/BS_OPNIT.o Release/Addnode.o


# _OBJ = SHAW24b/SHAW_WBCAN.FOR SHAW24b/SHAW_Photosyn.for SHAW24b/SHAW_LWRBAL.FOR DSSAT40/CERES-Sorghum/SG_PHENOL.FOR RZWQM/Rzpet.for RZWQM/DSSATNFIX.FOR SHAW24b/SHAW_VSLOPE.FOR SHAW24b/SHAW_TRANSC.FOR SHAW24b/SHAW_RESVK.FOR SHAW24b/SHAW_META.FOR SHAW24b/SHAW_CANTK.For DSSAT40/SUBSTOR-Potato/PT_NFACTO.for RZWQM/HERMES_Radia.for SHAW24b/SHAW_Weight.for SHAW24b/SHAW_SOILR.FOR SHAW24b/SHAW_SNOWHT.FOR SHAW24b/SHAW_LEAFT.for SHAW24b/SHAW_Conduc.for RZWQM/Rzchem.for Pmodel/KghaToKg.for DSSAT40/CROPGRO/FREEZE.FOR RZWQM/Comp2exp.for DSSAT40/Weather/SOLAR.FOR SHAW24b/SHAW_SWRBAL.FOR SHAW24b/SHAW_FSLOPE.FOR SHAW24b/SHAW_Frozen.for SHAW24b/SHAW_CANHUM.FOR RZWQM/Qckturf.for RZWQM/HERMES_SetupCrop.for DSSAT40/CSCER/CSUTS040.FOR SHAW24b/SHAW_TRANSR.FOR SHAW24b/SHAW_LWRMAT.FOR SHAW24b/SHAW_LWRATM.FOR SHAW24b/SHAW_Canlay.for SHAW24b/SHAW_Solvrad.for SHAW24b/SHAW_SNOWBC.FOR SHAW24b/SHAW_NWSNOW.FOR SHAW24b/SHAW_EBCAN.FOR RZWQM/Qckplnt.for DSSAT40/CERES-Millet/ML_PHASEI.FOR SHAW24b/SHAW_SWRSNO.FOR SHAW24b/SHAW_STMMLT.FOR SHAW24b/SHAW_RESGMC.FOR SHAW24b/SHAW_GOSHAW.FOR SHAW24b/SHAW_ADJUST.FOR DSSAT40/Generic-Pest/LINDM.FOR RZWQM/Horizon.for DSSAT40/CSCER/CSREA040.FOR SHAW24b/SHAW_SUMDT.FOR SHAW24b/SHAW_SNOALB.FOR SHAW24b/SHAW_RESHT.FOR DSSAT40/CERES-Sorghum/SG_ROOTGR.for DSSAT40/CERES-Sorghum/SG_NFACT.FOR DSSAT40/CERES-Millet/ML_rootgr.for SHAW24b/SHAW_Solar.for SHAW24b/SHAW_RESTK.FOR SHAW24b/SHAW_EBSNOW.FOR RZWQM/Rzplnt.for SHAW24b/SHAW_STAB.FOR SHAW24b/SHAW_QVSOIL.FOR SHAW24b/SHAW_EBSOIL.FOR SHAW24b/SHAW_Cloudy.for DSSAT40/Forage/for_lindm.for SHAW24b/SHAW_WBALNC.FOR SHAW24b/SHAW_source.for SHAW24b/SHAW_SOILHT.FOR SHAW24b/SHAW_LWRRES.FOR RZWQM/Rzpest.for RZWQM/Qcktree.for DSSAT40/CERES-Millet/ML_PHENOL.FOR DSSAT40/Forage/for_sdcomp.for SHAW24b/SHAW_WBSNOW.FOR SHAW24b/SHAW_SWRCAN.FOR SHAW24b/SHAW_SOILTK.FOR SHAW24b/SHAW_RESVAP.FOR SHAW24b/SHAW_TDMA.FOR SHAW24b/SHAW_QVSNOW.FOR SHAW24b/SHAW_LWRSNO.FOR SHAW24b/SHAW_BACKUP.FOR DSSAT40/CERES-Sorghum/SG_PHASEI.FOR Pmodel/KgtoKgha.for SHAW24b/SHAW_SNOMLT.FOR SHAW24b/SHAW_QVRES.FOR RZWQM/Rzrich.for RZWQM/Rzout.for RZWQM/readrzx.for Pmodel/Nodethick.for DSSAT40/Generic-Pest/IPPARM.for SHAW24b/SHAW_WBRES.FOR SHAW24b/SHAW_RESHUM.FOR SHAW24b/SHAW_FROST.FOR DSSAT40/CROPGRO/SDCOMP.FOR RZWQM/RZTEST.for RZWQM/REF_ET.FOR DSSAT40/Forage/for_ipparm.for SHAW24b/SHAW_Update.for SHAW24b/SHAW_RTDIST.for SHAW24b/SHAW_ENHANC.FOR SHAW24b/SHAW_ATstab.for RZWQM/RZTEMP.FOR GLEAMS/RZ-Erosion.for DSSAT40/SUBSTOR-Potato/PT_PHASEI.for DSSAT40/CERES-Millet/ML_NFACT.FOR Pmodel/Countdata.for RZWQM/Snowprms.for SHAW24b/SHAW_SNOWTK.FOR SHAW24b/SHAW_LWRCAN.FOR SHAW24b/SHAW_EBRES.FOR RZWQM/NO_N2O_RATIO.FOR DSSAT40/Input/LMATCH.FOR RZWQM/HERMES_SUCROS.for DSSAT40/Utilities/ModuleDefs.for Pmodel/Variable.for DSSAT40/Utilities/CsvOuts/csvlinklist.f90 DSSAT40/Utilities/OPSUM.for DSSAT40/Utilities/CsvOuts/csvoutput.f90 DSSAT40/Utilities/WBAL.for DSSAT40/Generic-Pest/ROOTDM.FOR Pmodel/PlantPuptake.for DSSAT40/Utilities/OpStemp.for DSSAT40/CERES-Maize/MZ_GROSUB.for DSSAT40/CERES-Millet/ML_GROSUB.FOR DSSAT40/Weather/HMET.for DSSAT40/Forage/for_oppest.for DSSAT40/Forage/for_nfix.for DSSAT40/CERES-Sugarbeet/BS_NFACTO.for DSSAT40/Generic-Pest/ASMDM.FOR DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR DSSAT40/SUBSTOR-Potato/PT_THTIME.for DSSAT40/Generic-Pest/OPPEST.FOR DSSAT40/CROPGRO/Opgrow.for DSSAT40/CROPGRO/OPETPHOT.FOR DSSAT40/CERES-Millet/ML_CERES.for DSSAT40/Generic-Pest/IPPROG.FOR DSSAT40/Forage/for_photo.for DSSAT40/Forage/for_ipprog.for DSSAT40/Forage/for_ch2oref.for DSSAT40/CERES-Sugarbeet/BS_PHENOL.for DSSAT40/CSCER/Alt_Plant.for Pmodel/TotalP.for DSSAT40/CERES-Sunflower/SF_OPGROW.FOR DSSAT40/CERES-Sunflower/SF_CERES.FOR DSSAT40/CROPGRO/PODDET.FOR DSSAT40/Utilities/OpSoilNC.for DSSAT40/CROPGRO/NFIX.FOR DSSAT40/Forage/for_pest.for DSSAT40/Forage/for_ippest.for Pmodel/Fertilizerfate.for DSSAT40/CERES-Sunflower/SF_PHENOL.for DSSAT40/CROPGRO/PODS.FOR DSSAT40/Generic-Pest/PESTCP.FOR DSSAT40/CERES-Maize/MZ_NFACTO.for DSSAT40/Input/IPVARC.for DSSAT40/Forage/for_phenol.for DSSAT40/Forage/for_mobil.for Pmodel/UPDATEPOOL.for DSSAT40/CERES-Sorghum/SG_OPHARV.FOR DSSAT40/CROPGRO/RESPIR.FOR DSSAT40/SUBSTOR-Potato/PT_OPHARV.for DSSAT40/Generic-Pest/PEST.FOR Pmodel/Openfile.for DSSAT40/Utilities/IPSOIL.FOR DSSAT40/CROPGRO/Ipphenol.for DSSAT40/Forage/for_pods.for DSSAT40/Forage/for_ipphenol.for DSSAT40/Forage/for_canopy.for DSSAT40/CSCER/CSCER040.FOR DSSAT40/Weather/WEATHR.for Pmodel/tillage.for DSSAT40/Generic-Pest/SEEDDM.FOR DSSAT40/CROPGRO/RStages.for DSSAT40/Utilities/OPVIEW.FOR DSSAT40/CERES-Maize/MZ_OPNIT.FOR Pmodel/HeatUnit.for DSSAT40/Forage/for_rootdm.for DSSAT40/Forage/for_opharv.for DSSAT40/Forage/for_freeze.for DSSAT40/CROPGRO/DEMAND.FOR DSSAT40/CROPGRO/CANOPY.FOR Pmodel/Breaknode.for DSSAT40/CROPGRO/SENES.FOR RZWQM/Rzmain.for DSSAT40/CROPGRO/PlantNBal.FOR DSSAT40/Weather/PET.FOR DSSAT40/Forage/for_plantnbal.for DSSAT40/Forage/for_hres_cgro.for Pmodel/Writeoutput.for DSSAT40/Utilities/UTILS.for RZWQM/Rzman.for DSSAT40/Utilities/READS.for DSSAT40/CROPGRO/PHOTO.FOR DSSAT40/Utilities/OPWBAL.for DSSAT40/CROPGRO/NUPTAK.FOR DSSAT40/Generic-Pest/IPPEST.FOR DSSAT40/Forage/for_seeddm.for DSSAT40/Forage/for_incomp.for DSSAT40/Forage/for_asmdm.for Pmodel/Cropdaycheck.for DSSAT40/CERES-Sugarbeet/BS_GROSUB.for DSSAT40/Utilities/SPSUBS.for DSSAT40/Utilities/SoilNBal.FOR DSSAT40/Utilities/ROOTWU.FOR DSSAT40/SUBSTOR-Potato/PT_PHENOL.for DSSAT40/CROPGRO/OPHARV.FOR DSSAT40/CERES-Maize/MZ_CERES.FOR DSSAT40/CERES-Millet/ML_opharv.for Pmodel/Initializepvar.for DSSAT40/Forage/for_veggr.for DSSAT40/Forage/for_respir.for DSSAT40/Forage/for_opgrow.for Pmodel/Fertilizer.for DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR DSSAT40/CROPGRO/VEGGR.FOR DSSAT40/CERES-Sunflower/SF_OPHARV.FOR RZWQM/Rzday.for DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for DSSAT40/CERES-Millet/ML_TILLSUB.FOR Pmodel/Labploss.for DSSAT40/CROPGRO/GROW.FOR DSSAT40/Forage/for_roots.for RZWQM/DSSATDRV.for DSSAT40/CSCER/CSCERES_Interface.for Pmodel/Writelog.for DSSAT40/CERES-Sorghum/SG_CERES.for DSSAT40/CERES-Sunflower/SF_ROOTS.FOR DSSAT40/SUBSTOR-Potato/PT_GROSUB.for DSSAT40/Weather/OPWEATH.FOR DSSAT40/CERES-Maize/MZ_NUPTAK.FOR DSSAT40/Forage/forage.for DSSAT40/Forage/for_grow.for Pmodel/Drplossrunoff.for DSSAT40/CROPGRO/CROPGRO.for DSSAT40/CERES-Sorghum/SG_GROSUB.FOR DSSAT40/CERES-Sunflower/SF_GROSUB.for DSSAT40/CROPGRO/ROOTS.for DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR DSSAT40/CROPGRO/PHENOL.FOR DSSAT40/CROPGRO/MOBIL.FOR DSSAT40/CERES-Millet/ML_NUPTAK.FOR DSSAT40/CSCER/HResCeres.for DSSAT40/Forage/for_pestcp.for DSSAT40/Forage/for_nuptak.for DSSAT40/Utilities/DATES.FOR DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR DSSAT40/Utilities/AUTHAR.FOR DSSAT40/Generic-Pest/VEGDM.FOR DSSAT40/SUBSTOR-Potato/PT_OPGROW.for Pmodel/PPlosstile.for DSSAT40/Input/OPGEN.FOR DSSAT40/CERES-Maize/MZ_PHENOL.for DSSAT40/CROPGRO/IPPLNT.FOR DSSAT40/Forage/for_vegdm.for DSSAT40/Forage/for_harv.for DSSAT40/Utilities/ERROR.for DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR Pmodel/Writeheader.for DSSAT40/CERES-Sunflower/SF_OPNIT.FOR Pmodel/PBALANCE.for Pmodel/Manurefate.for DSSAT40/Forage/for_rstages.for DSSAT40/Forage/for_opmob.for DSSAT40/Forage/for_demand.for Pmodel/Drplosstile.for DSSAT40/CERES-Sugarbeet/BS_CERES.FOR DSSAT40/Utilities/Warning.FOR Pmodel/PPlossrunoff.for Pmodel/Pflux.for DSSAT40/CERES-Maize/MZ_ROOTS.FOR DSSAT40/CERES-Maize/MZ_OPGROW.FOR Pmodel/ManureP.for DSSAT40/CROPGRO/HRes_CGRO.for DSSAT40/Forage/for_poddet.for DSSAT40/CROPGRO/ETPHR.for DSSAT40/Utilities/TRANS.FOR DSSAT40/Input/OPHEAD.FOR DSSAT40/CERES-Maize/MZ_OPHARV.FOR DSSAT40/CROPGRO/INCOMP.FOR DSSAT40/Forage/for_senmob.for DSSAT40/Forage/for_ipplnt.for DSSAT40/CERES-Sugarbeet/BS_HResCeres.for DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR DSSAT40/CERES-Sunflower/SF_NFACTO.for RZWQM/Rznutr.for DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for DSSAT40/Utilities/OPSTRESS.FOR DSSAT40/Forage/for_opview.for DSSAT40/Forage/for_dormancy.for DSSAT40/Forage/for_dormancy.for DSSAT40/CROPGRO/ETPHOT.for DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR Pmodel/Addnode.for
# OBJ = $(patsubst %/,$(ODIR)/,$(_OBJ))

# $(ODIR)/%.o: %.c 
# 	$(F77_FLAGGED) -c $@


Release/Rzmain.o: RZWQM/Rzmain.for
	$(F77_FLAGGED) -c RZWQM/Rzmain.for
Release/Addnode.o: Pmodel/Addnode.for
	$(F77_FLAGGED) -c Pmodel/Addnode.for
Release/Alt_Plant.o: DSSAT40/CSCER/Alt_Plant.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/Alt_Plant.for
Release/ASMDM.o: DSSAT40/Generic-Pest/ASMDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/ASMDM.FOR
Release/AUTHAR.o: DSSAT40/Utilities/AUTHAR.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/AUTHAR.FOR
Release/Breaknode.o: Pmodel/Breaknode.for
	$(F77_FLAGGED) -c Pmodel/Breaknode.for
Release/BS_CERES.o: DSSAT40/CERES-Sugarbeet/BS_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_CERES.FOR
Release/BS_GROSUB.o: DSSAT40/CERES-Sugarbeet/BS_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_GROSUB.for
Release/BS_HResCeres.o: DSSAT40/CERES-Sugarbeet/BS_HResCeres.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_HResCeres.for
Release/BS_NFACTO.o: DSSAT40/CERES-Sugarbeet/BS_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_NFACTO.for
Release/BS_NUPTAK.o: DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_NUPTAK.FOR
Release/BS_OPGROW.o: DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPGROW.FOR
Release/BS_OPHARV.o: DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPHARV.FOR
Release/BS_OPNIT.o: DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_OPNIT.FOR
Release/BS_PHENOL.o: DSSAT40/CERES-Sugarbeet/BS_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_PHENOL.for
Release/BS_ROOTS.o: DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sugarbeet/BS_ROOTS.FOR
Release/CANOPY.o: DSSAT40/CROPGRO/CANOPY.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/CANOPY.FOR
Release/Comp2exp.o: RZWQM/Comp2exp.for
	$(F77_FLAGGED) -c RZWQM/Comp2exp.for
Release/Countdata.o: Pmodel/Countdata.for
	$(F77_FLAGGED) -c Pmodel/Countdata.for
Release/Cropdaycheck.o: Pmodel/Cropdaycheck.for
	$(F77_FLAGGED) -c Pmodel/Cropdaycheck.for
Release/CROPGRO.o: DSSAT40/CROPGRO/CROPGRO.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/CROPGRO.for
Release/CSCER040.o: DSSAT40/CSCER/CSCER040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSCER040.FOR
Release/CSCERES_Interface.o: DSSAT40/CSCER/CSCERES_Interface.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSCERES_Interface.for
Release/CSREA040.o: DSSAT40/CSCER/CSREA040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSREA040.FOR
Release/CSUTS040.o: DSSAT40/CSCER/CSUTS040.FOR
	$(F77_FLAGGED) -c DSSAT40/CSCER/CSUTS040.FOR
Release/csvlinklist.o: DSSAT40/Utilities/CsvOuts/csvlinklist.f90
	$(F77_FLAGGED) -c DSSAT40/Utilities/CsvOuts/csvlinklist.f90
Release/csvoutput.o: DSSAT40/Utilities/CsvOuts/csvoutput.f90
	$(F77_FLAGGED) -c DSSAT40/Utilities/CsvOuts/csvoutput.f90
Release/DATES.o: DSSAT40/Utilities/DATES.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/DATES.FOR
Release/DEMAND.o: DSSAT40/CROPGRO/DEMAND.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/DEMAND.FOR
Release/Drplossrunoff.o: Pmodel/Drplossrunoff.for
	$(F77_FLAGGED) -c Pmodel/Drplossrunoff.for
Release/Drplosstile.o: Pmodel/Drplosstile.for
	$(F77_FLAGGED) -c Pmodel/Drplosstile.for
Release/DSSATDRV.o: RZWQM/DSSATDRV.for
	$(F77_FLAGGED) -c RZWQM/DSSATDRV.for
Release/DSSATNFIX.o: RZWQM/DSSATNFIX.FOR
	$(F77_FLAGGED) -c RZWQM/DSSATNFIX.FOR
Release/ERROR.o: DSSAT40/Utilities/ERROR.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/ERROR.for
Release/ETPHOT.o: DSSAT40/CROPGRO/ETPHOT.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ETPHOT.for
Release/ETPHR.o: DSSAT40/CROPGRO/ETPHR.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ETPHR.for
Release/Fertilizer.o: Pmodel/Fertilizer.for
	$(F77_FLAGGED) -c Pmodel/Fertilizer.for
Release/Fertilizerfate.o: Pmodel/Fertilizerfate.for
	$(F77_FLAGGED) -c Pmodel/Fertilizerfate.for
Release/for_asmdm.o: DSSAT40/Forage/for_asmdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_asmdm.for
Release/for_canopy.o: DSSAT40/Forage/for_canopy.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_canopy.for
Release/for_ch2oref.o: DSSAT40/Forage/for_ch2oref.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ch2oref.for
Release/for_demand.o: DSSAT40/Forage/for_demand.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_demand.for
Release/for_dormancy.o: DSSAT40/Forage/for_dormancy.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_dormancy.for
Release/for_freeze.o: DSSAT40/Forage/for_freeze.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_freeze.for
Release/for_grow.o: DSSAT40/Forage/for_grow.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_grow.for
Release/for_harv.o: DSSAT40/Forage/for_harv.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_harv.for
Release/for_hres_cgro.o: DSSAT40/Forage/for_hres_cgro.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_hres_cgro.for
Release/for_incomp.o: DSSAT40/Forage/for_incomp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_incomp.for
Release/for_ipparm.o: DSSAT40/Forage/for_ipparm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipparm.for
Release/for_ippest.o: DSSAT40/Forage/for_ippest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ippest.for
Release/for_ipphenol.o: DSSAT40/Forage/for_ipphenol.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipphenol.for
Release/for_ipplnt.o: DSSAT40/Forage/for_ipplnt.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipplnt.for
Release/for_ipprog.o: DSSAT40/Forage/for_ipprog.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_ipprog.for
Release/for_lindm.o: DSSAT40/Forage/for_lindm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_lindm.for
Release/for_mobil.o: DSSAT40/Forage/for_mobil.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_mobil.for
Release/for_nfix.o: DSSAT40/Forage/for_nfix.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_nfix.for
Release/for_nuptak.o: DSSAT40/Forage/for_nuptak.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_nuptak.for
Release/for_opgrow.o: DSSAT40/Forage/for_opgrow.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opgrow.for
Release/for_opharv.o: DSSAT40/Forage/for_opharv.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opharv.for
Release/for_opmob.o: DSSAT40/Forage/for_opmob.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opmob.for
Release/for_oppest.o: DSSAT40/Forage/for_oppest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_oppest.for
Release/for_opview.o: DSSAT40/Forage/for_opview.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_opview.for
Release/for_pest.o: DSSAT40/Forage/for_pest.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pest.for
Release/for_pestcp.o: DSSAT40/Forage/for_pestcp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pestcp.for
Release/for_phenol.o: DSSAT40/Forage/for_phenol.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_phenol.for
Release/for_photo.o: DSSAT40/Forage/for_photo.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_photo.for
Release/for_plantnbal.o: DSSAT40/Forage/for_plantnbal.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_plantnbal.for
Release/for_poddet.o: DSSAT40/Forage/for_poddet.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_poddet.for
Release/for_pods.o: DSSAT40/Forage/for_pods.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_pods.for
Release/for_respir.o: DSSAT40/Forage/for_respir.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_respir.for
Release/for_rootdm.o: DSSAT40/Forage/for_rootdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_rootdm.for
Release/for_roots.o: DSSAT40/Forage/for_roots.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_roots.for
Release/for_rstages.o: DSSAT40/Forage/for_rstages.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_rstages.for
Release/for_sdcomp.o: DSSAT40/Forage/for_sdcomp.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_sdcomp.for
Release/for_seeddm.o: DSSAT40/Forage/for_seeddm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_seeddm.for
Release/for_senmob.o: DSSAT40/Forage/for_senmob.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_senmob.for
Release/for_vegdm.o: DSSAT40/Forage/for_vegdm.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_vegdm.for
Release/for_veggr.o: DSSAT40/Forage/for_veggr.for
	$(F77_FLAGGED) -c DSSAT40/Forage/for_veggr.for
Release/forage.o: DSSAT40/Forage/forage.for
	$(F77_FLAGGED) -c DSSAT40/Forage/forage.for
Release/FREEZE.o: DSSAT40/CROPGRO/FREEZE.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/FREEZE.FOR
Release/GROW.o: DSSAT40/CROPGRO/GROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/GROW.FOR
Release/HeatUnit.o: Pmodel/HeatUnit.for
	$(F77_FLAGGED) -c Pmodel/HeatUnit.for
Release/HERMES_Radia.o: RZWQM/HERMES_Radia.for
	$(F77_FLAGGED) -c RZWQM/HERMES_Radia.for
Release/HERMES_SetupCrop.o: RZWQM/HERMES_SetupCrop.for
	$(F77_FLAGGED) -c RZWQM/HERMES_SetupCrop.for
Release/HERMES_SUCROS.o: RZWQM/HERMES_SUCROS.for
	$(F77_FLAGGED) -c RZWQM/HERMES_SUCROS.for
Release/HMET.o: DSSAT40/Weather/HMET.for
	$(F77_FLAGGED) -c DSSAT40/Weather/HMET.for
Release/Horizon.o: RZWQM/Horizon.for
	$(F77_FLAGGED) -c RZWQM/Horizon.for
Release/HRes_CGRO.o: DSSAT40/CROPGRO/HRes_CGRO.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/HRes_CGRO.for
Release/HResCeres.o: DSSAT40/CSCER/HResCeres.for
	$(F77_FLAGGED) -c DSSAT40/CSCER/HResCeres.for
Release/INCOMP.o: DSSAT40/CROPGRO/INCOMP.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/INCOMP.FOR
Release/Initializepvar.o: Pmodel/Initializepvar.for
	$(F77_FLAGGED) -c Pmodel/Initializepvar.for
Release/IPPARM.o: DSSAT40/Generic-Pest/IPPARM.for
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPARM.for
Release/IPPEST.o: DSSAT40/Generic-Pest/IPPEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPEST.FOR
Release/Ipphenol.o: DSSAT40/CROPGRO/Ipphenol.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/Ipphenol.for
Release/IPPLNT.o: DSSAT40/CROPGRO/IPPLNT.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/IPPLNT.FOR
Release/IPPROG.o: DSSAT40/Generic-Pest/IPPROG.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/IPPROG.FOR
Release/IPSOIL.o: DSSAT40/Utilities/IPSOIL.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/IPSOIL.FOR
Release/IPVARC.o: DSSAT40/Input/IPVARC.for
	$(F77_FLAGGED) -c DSSAT40/Input/IPVARC.for
Release/KghaToKg.o: Pmodel/KghaToKg.for
	$(F77_FLAGGED) -c Pmodel/KghaToKg.for
Release/KgtoKgha.o: Pmodel/KgtoKgha.for
	$(F77_FLAGGED) -c Pmodel/KgtoKgha.for
Release/Labploss.o: Pmodel/Labploss.for
	$(F77_FLAGGED) -c Pmodel/Labploss.for
Release/LINDM.o: DSSAT40/Generic-Pest/LINDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/LINDM.FOR
Release/LMATCH.o: DSSAT40/Input/LMATCH.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/LMATCH.FOR
Release/Manurefate.o: Pmodel/Manurefate.for
	$(F77_FLAGGED) -c Pmodel/Manurefate.for
Release/ManureP.o: Pmodel/ManureP.for
	$(F77_FLAGGED) -c Pmodel/ManureP.for
Release/ML_CERES.o: DSSAT40/CERES-Millet/ML_CERES.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_CERES.for
Release/ML_GROSUB.o: DSSAT40/CERES-Millet/ML_GROSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_GROSUB.FOR
Release/ML_NFACT.o: DSSAT40/CERES-Millet/ML_NFACT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_NFACT.FOR
Release/ML_NUPTAK.o: DSSAT40/CERES-Millet/ML_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_NUPTAK.FOR
Release/ML_opharv.o: DSSAT40/CERES-Millet/ML_opharv.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_opharv.for
Release/ML_PHASEI.o: DSSAT40/CERES-Millet/ML_PHASEI.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_PHASEI.FOR
Release/ML_PHENOL.o: DSSAT40/CERES-Millet/ML_PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_PHENOL.FOR
Release/ML_rootgr.o: DSSAT40/CERES-Millet/ML_rootgr.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_rootgr.for
Release/ML_TILLSUB.o: DSSAT40/CERES-Millet/ML_TILLSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Millet/ML_TILLSUB.FOR
Release/MOBIL.o: DSSAT40/CROPGRO/MOBIL.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/MOBIL.FOR
Release/ModuleDefs.o: DSSAT40/Utilities/ModuleDefs.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/ModuleDefs.for
Release/MZ_CERES.o: DSSAT40/CERES-Maize/MZ_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_CERES.FOR
Release/MZ_GROSUB.o: DSSAT40/CERES-Maize/MZ_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_GROSUB.for
Release/MZ_NFACTO.o: DSSAT40/CERES-Maize/MZ_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_NFACTO.for
Release/MZ_NUPTAK.o: DSSAT40/CERES-Maize/MZ_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_NUPTAK.FOR
Release/MZ_OPGROW.o: DSSAT40/CERES-Maize/MZ_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPGROW.FOR
Release/MZ_OPHARV.o: DSSAT40/CERES-Maize/MZ_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPHARV.FOR
Release/MZ_OPNIT.o: DSSAT40/CERES-Maize/MZ_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_OPNIT.FOR
Release/MZ_PHENOL.o: DSSAT40/CERES-Maize/MZ_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_PHENOL.for
Release/MZ_ROOTS.o: DSSAT40/CERES-Maize/MZ_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Maize/MZ_ROOTS.FOR
Release/NFIX.o: DSSAT40/CROPGRO/NFIX.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/NFIX.FOR
Release/NO_N2O_RATIO.o: RZWQM/NO_N2O_RATIO.FOR
	$(F77_FLAGGED) -c RZWQM/NO_N2O_RATIO.FOR
Release/Nodethick.o: Pmodel/Nodethick.for
	$(F77_FLAGGED) -c Pmodel/Nodethick.for
Release/NUPTAK.o: DSSAT40/CROPGRO/NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/NUPTAK.FOR
Release/Openfile.o: Pmodel/Openfile.for
	$(F77_FLAGGED) -c Pmodel/Openfile.for
Release/OPETPHOT.o: DSSAT40/CROPGRO/OPETPHOT.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/OPETPHOT.FOR
Release/OPGEN.o: DSSAT40/Input/OPGEN.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/OPGEN.FOR
Release/Opgrow.o: DSSAT40/CROPGRO/Opgrow.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/Opgrow.for
Release/OPHARV.o: DSSAT40/CROPGRO/OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/OPHARV.FOR
Release/OPHEAD.o: DSSAT40/Input/OPHEAD.FOR
	$(F77_FLAGGED) -c DSSAT40/Input/OPHEAD.FOR
Release/OPPEST.o: DSSAT40/Generic-Pest/OPPEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/OPPEST.FOR
Release/OpSoilNC.o: DSSAT40/Utilities/OpSoilNC.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OpSoilNC.for
Release/OpStemp.o: DSSAT40/Utilities/OpStemp.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OpStemp.for
Release/OPSTRESS.o: DSSAT40/Utilities/OPSTRESS.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPSTRESS.FOR
Release/OPSUM.o: DSSAT40/Utilities/OPSUM.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPSUM.for
Release/OPVIEW.o: DSSAT40/Utilities/OPVIEW.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPVIEW.FOR
Release/OPWBAL.o: DSSAT40/Utilities/OPWBAL.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/OPWBAL.for
Release/OPWEATH.o: DSSAT40/Weather/OPWEATH.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/OPWEATH.FOR
Release/PBALANCE.o: Pmodel/PBALANCE.for
	$(F77_FLAGGED) -c Pmodel/PBALANCE.for
Release/PEST.o: DSSAT40/Generic-Pest/PEST.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/PEST.FOR
Release/PESTCP.o: DSSAT40/Generic-Pest/PESTCP.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/PESTCP.FOR
Release/PET.o: DSSAT40/Weather/PET.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/PET.FOR
Release/Pflux.o: Pmodel/Pflux.for
	$(F77_FLAGGED) -c Pmodel/Pflux.for
Release/PHENOL.o: DSSAT40/CROPGRO/PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PHENOL.FOR
Release/PHOTO.o: DSSAT40/CROPGRO/PHOTO.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PHOTO.FOR
Release/PlantNBal.o: DSSAT40/CROPGRO/PlantNBal.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PlantNBal.FOR
Release/PlantPuptake.o: Pmodel/PlantPuptake.for
	$(F77_FLAGGED) -c Pmodel/PlantPuptake.for
Release/PODDET.o: DSSAT40/CROPGRO/PODDET.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PODDET.FOR
Release/PODS.o: DSSAT40/CROPGRO/PODS.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/PODS.FOR
Release/PPlossrunoff.o: Pmodel/PPlossrunoff.for
	$(F77_FLAGGED) -c Pmodel/PPlossrunoff.for
Release/PPlosstile.o: Pmodel/PPlosstile.for
	$(F77_FLAGGED) -c Pmodel/PPlosstile.for
Release/PT_GROSUB.o: DSSAT40/SUBSTOR-Potato/PT_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_GROSUB.for
Release/PT_NFACTO.o: DSSAT40/SUBSTOR-Potato/PT_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_NFACTO.for
Release/PT_NUPTAK.o: DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_NUPTAK.for
Release/PT_OPGROW.o: DSSAT40/SUBSTOR-Potato/PT_OPGROW.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_OPGROW.for
Release/PT_OPHARV.o: DSSAT40/SUBSTOR-Potato/PT_OPHARV.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_OPHARV.for
Release/PT_PHASEI.o: DSSAT40/SUBSTOR-Potato/PT_PHASEI.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_PHASEI.for
Release/PT_PHENOL.o: DSSAT40/SUBSTOR-Potato/PT_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_PHENOL.for
Release/PT_ROOTGR.o: DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_ROOTGR.for
Release/PT_SUBSTOR.o: DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_SUBSTOR.FOR
Release/PT_THTIME.o: DSSAT40/SUBSTOR-Potato/PT_THTIME.for
	$(F77_FLAGGED) -c DSSAT40/SUBSTOR-Potato/PT_THTIME.for
Release/Qckplnt.o: RZWQM/Qckplnt.for
	$(F77_FLAGGED) -c RZWQM/Qckplnt.for
Release/Qcktree.o: RZWQM/Qcktree.for
	$(F77_FLAGGED) -c RZWQM/Qcktree.for
Release/Qckturf.o: RZWQM/Qckturf.for
	$(F77_FLAGGED) -c RZWQM/Qckturf.for
Release/readrzx.o: RZWQM/readrzx.for
	$(F77_FLAGGED) -c RZWQM/readrzx.for
Release/READS.o: DSSAT40/Utilities/READS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/READS.for
Release/REF_ET.o: RZWQM/REF_ET.FOR
	$(F77_FLAGGED) -c RZWQM/REF_ET.FOR
Release/RESPIR.o: DSSAT40/CROPGRO/RESPIR.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/RESPIR.FOR
Release/ROOTDM.o: DSSAT40/Generic-Pest/ROOTDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/ROOTDM.FOR
Release/ROOTS.o: DSSAT40/CROPGRO/ROOTS.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/ROOTS.for
Release/ROOTWU.o: DSSAT40/Utilities/ROOTWU.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/ROOTWU.FOR
Release/RStages.o: DSSAT40/CROPGRO/RStages.for
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/RStages.for
Release/RZ-Erosion.o: GLEAMS/RZ-Erosion.for
	$(F77_FLAGGED) -c GLEAMS/RZ-Erosion.for
Release/Rzchem.o: RZWQM/Rzchem.for
	$(F77_FLAGGED) -c RZWQM/Rzchem.for
Release/Rzday.o: RZWQM/Rzday.for
	$(F77_FLAGGED) -c RZWQM/Rzday.for
Release/Rzmain.o: RZWQM/Rzmain.for
	$(F77_FLAGGED) -c RZWQM/Rzmain.for
Release/Rzman.o: RZWQM/Rzman.for
	$(F77_FLAGGED) -c RZWQM/Rzman.for
Release/Rznutr.o: RZWQM/Rznutr.for
	$(F77_FLAGGED) -c RZWQM/Rznutr.for
Release/Rzout.o: RZWQM/Rzout.for
	$(F77_FLAGGED) -c RZWQM/Rzout.for
Release/Rzpest.o: RZWQM/Rzpest.for
	$(F77_FLAGGED) -c RZWQM/Rzpest.for
Release/Rzpet.o: RZWQM/Rzpet.for
	$(F77_FLAGGED) -c RZWQM/Rzpet.for
Release/Rzplnt.o: RZWQM/Rzplnt.for
	$(F77_FLAGGED) -c RZWQM/Rzplnt.for
Release/Rzrich.o: RZWQM/Rzrich.for
	$(F77_FLAGGED) -c RZWQM/Rzrich.for
Release/RZTEMP.o: RZWQM/RZTEMP.FOR
	$(F77_FLAGGED) -c RZWQM/RZTEMP.FOR
Release/RZTEST.o: RZWQM/RZTEST.for
	$(F77_FLAGGED) -c RZWQM/RZTEST.for
Release/SDCOMP.o: DSSAT40/CROPGRO/SDCOMP.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/SDCOMP.FOR
Release/SEEDDM.o: DSSAT40/Generic-Pest/SEEDDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/SEEDDM.FOR
Release/SENES.o: DSSAT40/CROPGRO/SENES.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/SENES.FOR
Release/SF_CERES.o: DSSAT40/CERES-Sunflower/SF_CERES.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_CERES.FOR
Release/SF_GROSUB.o: DSSAT40/CERES-Sunflower/SF_GROSUB.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_GROSUB.for
Release/SF_NFACTO.o: DSSAT40/CERES-Sunflower/SF_NFACTO.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_NFACTO.for
Release/SF_NUPTAK.o: DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_NUPTAK.FOR
Release/SF_OPGROW.o: DSSAT40/CERES-Sunflower/SF_OPGROW.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPGROW.FOR
Release/SF_OPHARV.o: DSSAT40/CERES-Sunflower/SF_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPHARV.FOR
Release/SF_OPNIT.o: DSSAT40/CERES-Sunflower/SF_OPNIT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_OPNIT.FOR
Release/SF_PHENOL.o: DSSAT40/CERES-Sunflower/SF_PHENOL.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_PHENOL.for
Release/SF_ROOTS.o: DSSAT40/CERES-Sunflower/SF_ROOTS.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sunflower/SF_ROOTS.FOR
Release/SG_CERES.o: DSSAT40/CERES-Sorghum/SG_CERES.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_CERES.for
Release/SG_GROSUB.o: DSSAT40/CERES-Sorghum/SG_GROSUB.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_GROSUB.FOR
Release/SG_NFACT.o: DSSAT40/CERES-Sorghum/SG_NFACT.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_NFACT.FOR
Release/SG_NUPTAK.o: DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_NUPTAK.FOR
Release/SG_OPHARV.o: DSSAT40/CERES-Sorghum/SG_OPHARV.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_OPHARV.FOR
Release/SG_PHASEI.o: DSSAT40/CERES-Sorghum/SG_PHASEI.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_PHASEI.FOR
Release/SG_PHENOL.o: DSSAT40/CERES-Sorghum/SG_PHENOL.FOR
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_PHENOL.FOR
Release/SG_ROOTGR.o: DSSAT40/CERES-Sorghum/SG_ROOTGR.for
	$(F77_FLAGGED) -c DSSAT40/CERES-Sorghum/SG_ROOTGR.for
Release/SHAW_ADJUST.o: SHAW24b/SHAW_ADJUST.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_ADJUST.FOR
Release/SHAW_ATstab.o: SHAW24b/SHAW_ATstab.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_ATstab.for
Release/SHAW_BACKUP.o: SHAW24b/SHAW_BACKUP.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_BACKUP.FOR
Release/SHAW_CANHUM.o: SHAW24b/SHAW_CANHUM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_CANHUM.FOR
Release/SHAW_Canlay.o: SHAW24b/SHAW_Canlay.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Canlay.for
Release/SHAW_CANTK.o: SHAW24b/SHAW_CANTK.For
	$(F77_FLAGGED) -c SHAW24b/SHAW_CANTK.For
Release/SHAW_Cloudy.o: SHAW24b/SHAW_Cloudy.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Cloudy.for
Release/SHAW_Conduc.o: SHAW24b/SHAW_Conduc.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Conduc.for
Release/SHAW_EBCAN.o: SHAW24b/SHAW_EBCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBCAN.FOR
Release/SHAW_EBRES.o: SHAW24b/SHAW_EBRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBRES.FOR
Release/SHAW_EBSNOW.o: SHAW24b/SHAW_EBSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBSNOW.FOR
Release/SHAW_EBSOIL.o: SHAW24b/SHAW_EBSOIL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_EBSOIL.FOR
Release/SHAW_ENHANC.o: SHAW24b/SHAW_ENHANC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_ENHANC.FOR
Release/SHAW_FROST.o: SHAW24b/SHAW_FROST.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_FROST.FOR
Release/SHAW_Frozen.o: SHAW24b/SHAW_Frozen.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Frozen.for
Release/SHAW_FSLOPE.o: SHAW24b/SHAW_FSLOPE.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_FSLOPE.FOR
Release/SHAW_GOSHAW.o: SHAW24b/SHAW_GOSHAW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_GOSHAW.FOR
Release/SHAW_LEAFT.o: SHAW24b/SHAW_LEAFT.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_LEAFT.for
Release/SHAW_LWRATM.o: SHAW24b/SHAW_LWRATM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRATM.FOR
Release/SHAW_LWRBAL.o: SHAW24b/SHAW_LWRBAL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRBAL.FOR
Release/SHAW_LWRCAN.o: SHAW24b/SHAW_LWRCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRCAN.FOR
Release/SHAW_LWRMAT.o: SHAW24b/SHAW_LWRMAT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRMAT.FOR
Release/SHAW_LWRRES.o: SHAW24b/SHAW_LWRRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRRES.FOR
Release/SHAW_LWRSNO.o: SHAW24b/SHAW_LWRSNO.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_LWRSNO.FOR
Release/SHAW_META.o: SHAW24b/SHAW_META.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_META.FOR
Release/SHAW_NWSNOW.o: SHAW24b/SHAW_NWSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_NWSNOW.FOR
Release/SHAW_Photosyn.o: SHAW24b/SHAW_Photosyn.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Photosyn.for
Release/SHAW_QVRES.o: SHAW24b/SHAW_QVRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVRES.FOR
Release/SHAW_QVSNOW.o: SHAW24b/SHAW_QVSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVSNOW.FOR
Release/SHAW_QVSOIL.o: SHAW24b/SHAW_QVSOIL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_QVSOIL.FOR
Release/SHAW_RESGMC.o: SHAW24b/SHAW_RESGMC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESGMC.FOR
Release/SHAW_RESHT.o: SHAW24b/SHAW_RESHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESHT.FOR
Release/SHAW_RESHUM.o: SHAW24b/SHAW_RESHUM.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESHUM.FOR
Release/SHAW_RESTK.o: SHAW24b/SHAW_RESTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESTK.FOR
Release/SHAW_RESVAP.o: SHAW24b/SHAW_RESVAP.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESVAP.FOR
Release/SHAW_RESVK.o: SHAW24b/SHAW_RESVK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_RESVK.FOR
Release/SHAW_RTDIST.o: SHAW24b/SHAW_RTDIST.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_RTDIST.for
Release/SHAW_SNOALB.o: SHAW24b/SHAW_SNOALB.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOALB.FOR
Release/SHAW_SNOMLT.o: SHAW24b/SHAW_SNOMLT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOMLT.FOR
Release/SHAW_SNOWBC.o: SHAW24b/SHAW_SNOWBC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWBC.FOR
Release/SHAW_SNOWHT.o: SHAW24b/SHAW_SNOWHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWHT.FOR
Release/SHAW_SNOWTK.o: SHAW24b/SHAW_SNOWTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SNOWTK.FOR
Release/SHAW_SOILHT.o: SHAW24b/SHAW_SOILHT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILHT.FOR
Release/SHAW_SOILR.o: SHAW24b/SHAW_SOILR.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILR.FOR
Release/SHAW_SOILTK.o: SHAW24b/SHAW_SOILTK.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SOILTK.FOR
Release/SHAW_Solar.o: SHAW24b/SHAW_Solar.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Solar.for
Release/SHAW_Solvrad.o: SHAW24b/SHAW_Solvrad.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Solvrad.for
Release/SHAW_source.o: SHAW24b/SHAW_source.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_source.for
Release/SHAW_STAB.o: SHAW24b/SHAW_STAB.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_STAB.FOR
Release/SHAW_STMMLT.o: SHAW24b/SHAW_STMMLT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_STMMLT.FOR
Release/SHAW_SUMDT.o: SHAW24b/SHAW_SUMDT.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SUMDT.FOR
Release/SHAW_SWRBAL.o: SHAW24b/SHAW_SWRBAL.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRBAL.FOR
Release/SHAW_SWRCAN.o: SHAW24b/SHAW_SWRCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRCAN.FOR
Release/SHAW_SWRSNO.o: SHAW24b/SHAW_SWRSNO.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_SWRSNO.FOR
Release/SHAW_TDMA.o: SHAW24b/SHAW_TDMA.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TDMA.FOR
Release/SHAW_TRANSC.o: SHAW24b/SHAW_TRANSC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TRANSC.FOR
Release/SHAW_TRANSR.o: SHAW24b/SHAW_TRANSR.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_TRANSR.FOR
Release/SHAW_Update.o: SHAW24b/SHAW_Update.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Update.for
Release/SHAW_VSLOPE.o: SHAW24b/SHAW_VSLOPE.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_VSLOPE.FOR
Release/SHAW_WBALNC.o: SHAW24b/SHAW_WBALNC.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBALNC.FOR
Release/SHAW_WBCAN.o: SHAW24b/SHAW_WBCAN.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBCAN.FOR
Release/SHAW_WBRES.o: SHAW24b/SHAW_WBRES.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBRES.FOR
Release/SHAW_WBSNOW.o: SHAW24b/SHAW_WBSNOW.FOR
	$(F77_FLAGGED) -c SHAW24b/SHAW_WBSNOW.FOR
Release/SHAW_Weight.o: SHAW24b/SHAW_Weight.for
	$(F77_FLAGGED) -c SHAW24b/SHAW_Weight.for
Release/Snowprms.o: RZWQM/Snowprms.for
	$(F77_FLAGGED) -c RZWQM/Snowprms.for
Release/SoilNBal.o: DSSAT40/Utilities/SoilNBal.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/SoilNBal.FOR
Release/SOLAR.o: DSSAT40/Weather/SOLAR.FOR
	$(F77_FLAGGED) -c DSSAT40/Weather/SOLAR.FOR
Release/SPSUBS.o: DSSAT40/Utilities/SPSUBS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/SPSUBS.for
Release/tillage.o: Pmodel/tillage.for
	$(F77_FLAGGED) -c Pmodel/tillage.for
Release/TotalP.o: Pmodel/TotalP.for
	$(F77_FLAGGED) -c Pmodel/TotalP.for
Release/TRANS.o: DSSAT40/Utilities/TRANS.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/TRANS.FOR
Release/UPDATEPOOL.o: Pmodel/UPDATEPOOL.for
	$(F77_FLAGGED) -c Pmodel/UPDATEPOOL.for
Release/UTILS.o: DSSAT40/Utilities/UTILS.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/UTILS.for
Release/Variable.o: Pmodel/Variable.for
	$(F77_FLAGGED) -c Pmodel/Variable.for
Release/VEGDM.o: DSSAT40/Generic-Pest/VEGDM.FOR
	$(F77_FLAGGED) -c DSSAT40/Generic-Pest/VEGDM.FOR
Release/VEGGR.o: DSSAT40/CROPGRO/VEGGR.FOR
	$(F77_FLAGGED) -c DSSAT40/CROPGRO/VEGGR.FOR
Release/Warning.o: DSSAT40/Utilities/Warning.FOR
	$(F77_FLAGGED) -c DSSAT40/Utilities/Warning.FOR
Release/WBAL.o: DSSAT40/Utilities/WBAL.for
	$(F77_FLAGGED) -c DSSAT40/Utilities/WBAL.for
Release/WEATHR.o: DSSAT40/Weather/WEATHR.for
	$(F77_FLAGGED) -c DSSAT40/Weather/WEATHR.for
Release/Writeheader.o: Pmodel/Writeheader.for
	$(F77_FLAGGED) -c Pmodel/Writeheader.for
Release/Writelog.o: Pmodel/Writelog.for
	$(F77_FLAGGED) -c Pmodel/Writelog.for
Release/Writeoutput.o: Pmodel/Writeoutput.for
	$(F77_FLAGGED) -c Pmodel/Writeoutput.for

.PHONY: clean
clean: 
	rm -f $(ODIR)/*.o $(ODIR)/*.mod
