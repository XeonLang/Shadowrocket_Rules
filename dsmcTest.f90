MODULE CONST1
    IMPLICIT NONE
    INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
CONTAINS
    SUBROUTINE CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        OPEN (1,FILE='PARAMETER.DAT',STATUS='OLD',FORM='FORMATTED')
        READ (1,*)
        READ (1,*) MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CLOSE (1)
    END SUBROUTINE CONST
END MODULE CONST1

MODULE MOLS2
    USE CONST1
    IMPLICIT NONE
    INTEGER :: NM
    INTEGER,ALLOCATABLE :: IPL(:),IPS(:),IR(:)
    REAL,ALLOCATABLE :: PP(:,:),PV(:,:)
CONTAINS
    SUBROUTINE ALL1
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(IPL(MNM),IPS(MNMS),IR(MNM),PP(2,MNM),PV(3,MNM))
    END SUBROUTINE ALL1
END MODULE MOLS2

MODULE MOLSR
    USE CONST1
    IMPLICIT NONE
    REAL,ALLOCATABLE :: PR(:)
CONTAINS
    SUBROUTINE ALL2
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(PR(MNMR))
    END SUBROUTINE ALL2
END MODULE MOLSR

MODULE MOLD
    USE CONST1	
    IMPLICIT NONE
    INTEGER :: NMB
    INTEGER,ALLOCATABLE :: IPLB(:),IPSB(:),IRB(:)
    REAL,ALLOCATABLE :: PPB(:,:),PVB(:,:),PRB(:)
CONTAINS
    SUBROUTINE ALL3
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(IPLB(MNB),IPSB(MNB),IRB(MNB),PPB(2,MNB),PVB(3,MNB),PRB(MNB))
    END SUBROUTINE ALL3
END MODULE MOLD

MODULE CELL2
    USE CONST1
    IMPLICIT NONE
    INTEGER :: NCX,NCY,IFCX,IFCY
    REAL :: CWRX,CWRY,APX,RPX,APY,RPY
    INTEGER,ALLOCATABLE :: IC(:,:,:),ISC(:),ISCG(:,:,:),IG(:,:)
    REAL,ALLOCATABLE :: CC(:),CG(:,:),CCG(:,:,:,:)
CONTAINS
    SUBROUTINE ALL4
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(IC(2,MNC,MNSG),ISC(MNSC),ISCG(2,MNSC,MNSG),IG(2,MNSG),CC(MNC),CG(6,MNC),CCG(2,MNC,MNSG,MNSG))
    END SUBROUTINE ALL4
END MODULE CELL2

MODULE GAS
    USE CONST1
    IMPLICIT NONE
    INTEGER,ALLOCATABLE :: ISP(:)
    REAL,ALLOCATABLE :: SP(:,:),SPM(:,:,:)
CONTAINS
    SUBROUTINE ALL5
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(ISP(MNSP),SP(5,MNSP),SPM(6,MNSP,MNSP))
    END SUBROUTINE ALL5
END MODULE GAS

MODULE GASR
    USE CONST1
    IMPLICIT NONE
    INTEGER,ALLOCATABLE :: ISPR(:,:)
    REAL,ALLOCATABLE :: SPR(:,:,:),CT(:)
CONTAINS
    SUBROUTINE ALL6
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(ISPR(3,MNSP),SPR(3,MNSP,MNSP),CT(MNC))
    END SUBROUTINE ALL6
END MODULE GASR

MODULE SAMPJET
    USE CONST1
    IMPLICIT NONE
    INTEGER :: NPR,NSMP,ISPD
    REAL*8 :: MOVT,NCOL,SELT,SEPT
    REAL :: TIME,FND,FFND,FTMP,TIMI,VFW
    REAL*8,ALLOCATABLE :: COL(:,:),CS(:,:,:)
    REAL,ALLOCATABLE :: FSP(:)
CONTAINS
    SUBROUTINE ALL7
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(COL(MNSP,MNSP),CS(9,MNC,MNSP),FSP(MNSP))
    END SUBROUTINE ALL7
END MODULE SAMPJET

MODULE SAMPR
    USE CONST1
    IMPLICIT NONE
    REAL*8,ALLOCATABLE :: CSR(:,:)
CONTAINS
    SUBROUTINE ALL8
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(CSR(MNC,MNSP))
    END SUBROUTINE ALL8
END MODULE SAMPR

MODULE SAMPS
    USE CONST1
    IMPLICIT NONE
    REAL*8,ALLOCATABLE :: CSS(:,:,:)
CONTAINS
    SUBROUTINE ALL9
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(CSS(9,MNSE,MNSP))
    END SUBROUTINE ALL9
END MODULE SAMPS

MODULE COMP
    IMPLICIT NONE
    INTEGER :: NIS,NSP,NPT
    REAL :: FNUM,DTM
END MODULE COMP

MODULE GEOMJET
    USE CONST1
    IMPLICIT NONE
    INTEGER :: NSCX,NSCY,IB(4),ISURF(4),LIMS(4,3),IIS,ISG,IJET,LIMJ(3),LFLX,LFLY,IWF
    REAL :: CB(4),TSURF(4),TMPJ,FNDJ,FUJ,FVJ,FWJ,CW,FW,CH,FH,WJ,ALPI(4),ALPN(4),ALPT(4),RWF,WSURF(4)
    REAL,ALLOCATABLE :: FSPJ(:),AMEJ(:,:),AMRJ(:,:),AME(:,:,:),AMR(:,:,:),&
		VEFu(:),VEFv(:),VEFw(:),rou_bound(:),T_bound(:)
CONTAINS
    SUBROUTINE ALL10
        IMPLICIT NONE
        INTEGER :: MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS
        CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
        ALLOCATE(FSPJ(MNSP),AMEJ(MNSP,MBC),AMRJ(MNSP,MBC),AME(4,MNSP,MBC),AMR(4,MNSP,MBC),&
			VEFu(MBC),VEFv(MBC),VEFw(MBC),rou_bound(MBC),T_bound(MBC))
    END SUBROUTINE ALL10
END MODULE GEOMJET

MODULE CONST2
    IMPLICIT NONE
    REAL :: PI,SPI,BOLTZ
END MODULE CONST2

MODULE ELAST
    IMPLICIT NONE
    INTEGER :: L,M,LS,MS,MM,NN,N
    REAL :: VRC(3),VRR,VR,CVR
END MODULE ELAST

!DSMCJET.FOR

PROGRAM DSMCJET

!--axially symmetric steady flow program (axis at y=0, y is radius)
!--x is in the axial direction, z is in the circumferential direction
!----the flowfield is a rectangular region in the x-y plane
!----radial weighting factors may be employed
!----the cells and sub-cells are rectangular
!----the cell spacing may be a geometric progression in each direction
!----each of the four boundaries may be either
!------a uniform stream,
!------a plane of symmetry (only for boundaries normal to the axis)
!------a vacuum, or
!------the axis
!----there may be one or two one-sided surfaces
!------they must lie along a cell boundary
!------they may lie along a flow boundary
!------they may be back-to-back to form one double-sided surface
!------the surface reflection may be diffuse, specular,
!------or follow the Cercignani-Lampis-Lord (CLL) model
!------the surface may be moving in the circumferential direction
!------(the CLL model cannot be used with circumferential velocities)
!----there may be one uniform jet
!------this must lie along a cell boundary

!--SI units are used throughout

!-------------------------DESCRIPTION OF DATA---------------------------

!--PARAMETER variables must be consistent with data in subroutine DATAJET

!--IWF 0,1 for no weighting factors, or factors proportional to radius
!--RWF the reference radius below which the weighting factors are unity
!----required only for IWF=1, very small values are not recommended

!--NCX the number of cells in the x direction (cell columns)
!--NCY the number of cells in the y direction (cell rows)
!----(MNC must be equal to or greater than NCX*NCY)

!--NSCX the number of sub-cells per cell in the x direction
!--NSCY the number of sub-cells per cell in the y direction
!----(MNSC must be at least MNC*NSCX*NSCY)

!--IFCX set to 0 or 1 for uniform or non-uniform cell widths in x dirn.
!---if IFCX=1, set CWRX as the ratio of the cell width at the outer x
!----boundary to that at the inner x boundary (default 0)
!--IFCY and CWRY similar to IFCX and CWRX for the y direction

!--IIS 0 if there is no stream, 1 if there is a uniform stream
!--ISG 0 if there is a stream and the initial state is a vacuum, or
!----1 if the initial state is a uniform stream, or

!--if part of the rectangular region is excluded from the flow, set
!--LFLX if positive no flow below where LFLY condition is also satisfied
!------ if negative no flow above where LFLY condition is also satisfied
!--LFLY if positive no flow below where LFLX condition is also satisfied
!------ if negative no flow above where LFLX condition is also satisfied
!--for example, if LFLX=3 and LFLY=-2, there is no stream in the corner
!----below cell column 3 and above cell row 2
!--if no flow is excluded, LFLX,Y are automatically set to 0

!--FTMP the stream temperature if IIS=1, or a temperature characteristic
!----of the flow otherwise (if FTMP is not set for IIS= 0, the
!----default value of 273 is used to set the initial value of CCG(1

!--FND the initial number density for IIS=1
!----or need not be set for IIS=0

!--VFX the stream velocity in the x direction
!--need not be set for IIS=0

!--FSP(L) the fraction (by number) of species L in the initial stream
!----a value is requred for each species, but need not be set for IIS=0

!--FNUM the number of real mols. represented by each simulated molecule

!--DTM the time step over which the motion and collisions are uncoupled

!--the following data is required for each boundary
!----K=1 for the lower value of x
!----K=2 for the higher value of x
!----K=3 for the lower value of y
!--if a boundary is on the axis, it must be boundary 3
!----K=4 for the higher value of y

!--CB(K) the coordinate of the boundary (x for K=1 or 2, y for K=3 or 4)

!--IB(K) the type code of the boundary
!----1 for stream, 2 for plane of symmetry, 3 for vacuum, 4 for axis

!--ISURF(K) with K=1, 2 for the two surfaces
!----0 if there is no surface
!----1 if the surface normal is in the positive y direction
!----2 if the surface normal is in the negative y direction
!----3 if the surface normal is in the positive x direction
!----4 if the surface normal is in the negative x direction

!--the following surface data is required if ISURF(K) > 0

!--LIMS(K,L) with K as before, and the surface lies along the
!----lower boundary of the cell row or column given by L=1
!----one edge of the surface is at the lower edge of the row or column
!----given by L=2, and the other is at the upper edge of the row or
!----column given by L=3

!--WSURF(K) the circumferential velocity of the surface
!----applies only to surfaces parallel to the axis and
!----these surfaces must be diffusely reflecting
!--TSURF(K) the temperature of the surface
!----if TSURF is negative, the reflection is specular
!----if TSURF is positive, the following must also be specified:
!--ALPI(K) should be -1. for diffuse reflection, or between 0 and 1
!----for the rotational energy accommodation coefficient for CLL model
!--ALPN(K) the normal momentum accommodation coeff. in the CLL model
!--ALPT(K) the tangential momentum accommodation coeff. in the CLL model
!----(if ALPI is not set explicitly, it is set to default value of -1.)

!--end of surface data

!--IJET  0 if there is no jet, or
!----1 if the jet is in the positive y direction
!----2 if the jet is in the negative y direction
!----3 if the jet is in the positive x direction
!----4 if the jet is in the negative x direction

!--if IJET > 0, the following data is required

!--LIMJ(L) the jet efflux plane lies along the
!----lower boundary of the cell row or column given by L=1
!----one edge of the plane is at the lower edge of the row or column
!----given by L=2, and the other is at the upper edge of the row or
!----column given by L=3

!--TMPJ the jet temperature

!--FNDJ the number density of the jet

!--FVJ the jet velocity

!--FSPJ(L) the fraction (by number) of species L in the jet
!----a value is requred for each species

!--end of jet data

!--ISPD (required only for gas mixtures) set to 0 if the diameter,
!----viscosity exponent, and VSS scattering parameter for the
!----cross-collisions are to be set to the mean values, or
!----set to 1 if these quantities are to be set as data

!--the following data must be repeated for each species (L=1 to MNSP)

!--SP(1,L) the reference diameter
!--SP(2,L) the reference temperature
!--SP(3,L) the viscosity temperature power law
!--SP(4,L) the reciprocal of the VSS scattering parameter (1. for VHS)
!--SP(5,L) the molecular mass

!--ISP(L) the collision sampling group in which the species lies
!----this must be LE.MNSC (not required if MNSG=1)

!--ISPR(1,L) the number of rotational degrees of freedom
!--ISPR(2,L) 0, 1 for constant, polynomial rotational relaxation number
!--ISPR(3,L) 0, 1 for common or collision partner species dependent
!----rotational relaxation rate

!--SPR(1,L,K) the constant value, or constant in the polynomial for Zr
!----in a collision of species L with species K
!--the following two items are required only if ISPR(2,L)=1
!--SPR(2,L,K) the coefficient of temperature in the polynomial
!--SPR(3,L,K) the coefficient of temperature squared in the polynomial

!--end of data for the individual species

!--the following data on the cross-collisions is required only if ISPD=1
!--then only for L.NE.M, but L,M data must be repeated for M,L

!--SPM(1,L,M) the reference diameter for species L-M collisions
!--SPM(2,L,M) the reference temperature for species L-M collisions
!--SPM(3,L,M) the viscosity temperature power law for species L-M colls.
!--SPM(4,L,M) the reciprocal of the VSS scattering parameter

!--end of species data

!--NIS the number of DTM time steps between samplings

!--NSP the number of samples between prints

!--NPS the number of prints to the assumed start of steady flow

!--NPT the number of prints to STOP

!-----------------------------------------------------------------------
	
    USE CONST1
    USE MOLS2
    USE MOLSR
    USE MOLD
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE SAMPR
    USE SAMPS
    USE COMP
    USE GEOMJET
    USE CONST2
    IMPLICIT NONE
!--MNMR 1 if all molecules are monatomic, MNM otherwise
!--MNMS 1 if there is only one species, MNM for a gas mixture
!--MBC the maximum number of cell divisions along any entry boundary
!--MNB the maximum number of molecules in the duplication delay buffer
!--other variables as defined in DSMC2.FOR

!--CS(8 the sample (unweighted) number
!--CS(9,1 the square of the sample (all species)
!--other variables as defined in DSMC0.FOR, but all values are weighted

!--CSR(M,L) the sum of the rotational energy of species L in cell M

    !INTEGER :: NQL,NQLS,JJJ,III
    INTEGER :: JJJ,III

!--CSS(N,M,L) sampled info. on the molecules striking the boundaries
!----M is the code number of the element; L is the species
!----N=1 the weighted number sum
!----N=2 the sum of the normal momentum of the incident molecules
!----N=3 the sum of the normal momentum for the reflected molecules
!----N=4 the sum of the incident tangential momentum
!----N=5 the sum of the incident translational energy
!----N=6 the sum of the reflected translational energy
!----N=7 the sum of the incident rotational energy
!----N=8 the sum of the reflected rotational energy
!----N=9 the sum of the reflected tangential momentum

!--variables as defined in DSMC0.FOR except that PP( is two-dimensional

!--PR(M) is the rotational energy of molecule M

!--NMB the number of molecules in the duplication delay buffer

!--IFCX,IFCY 0,1 for uniform, geometric progression for cell width
!--CWRX, CWRY the ratio of cell width, height at higher valued boundary
!----to that at the lower valued boundary
!--other variables as defined in DSMC0.FOR except that
!----CG(4,5,6 for y correspond to CG(1,2,3 for x

!--variables as defined in DSMC0.FOR

!--variables as defined in DSMC0R.FOR

!--VFX stream velocity component in X direction
!--other variables as defined in DSMC0.FOR

!--variables as defined in DSMC0.FOR

!--IIS 0, 1 if the initial flow is a vacuum, uniform stream
!--NSCX,NSCY the number of sub-cells per cell in x,y directions
!--CB(N) the location of the boundary
!----N=1,2 for lower, higher value of x
!----N=3,4 for lower, higher value of y
!--IB(N) N=1 to 4 as above,  the type code for the boundary
!--ISURF(K) 0 for no surface, otherwise direction of normal to surface K
!--LIMS(K,3) defines the location of the surface
!--TSURF(K) the temperature of the surface (negative for specular refl.)
!--WSURF(K) the circumferential velocity of the surface
!--IJET 0 for no jet, otherwise the jet direction
!--LIMJ(K) defines the location of the jet
!--TMPJ the jet temperature
!--FNDJ the number density of the jet
!--FVJ the jet velocity
!--FSPJ(L) the fraction of species L in the jet
!--AME(N,L,K) number of molecules of species L that enter element K of
!----side N each DTM
!--AMR(N,L,K) the remainder associated with AME
!--AMEJ(L,K) number of molecules of species L that enter el. K of jet
!--AMRJ(L,K) the remainder associated with AMEJ
!--CW the cell width for uniform cells
!--FW the flow width
!--ALPI(K) the accommodation coefficient for rotational energy for CLL
!----model this should be negative if the reflection is to be diffuse
!--ALPN(K) the CLL accommodation coefficient for normal momentum
!--ALPT(K) the CLL accommodation coefficient for tangential momentum
!--IWF 0,1 for no weighting factors, factors proportional to radius
!--RWF the reference radius for weighting factors
!----if IWF=1, a simulated molecule at radius r represents r*FNUM/RWF
!------real molecules

!--variables as defined in DSMC0.FOR

    CALL CONST(MNM,MNMR,MNMS,MNC,MNSC,MNSP,MNSG,MNSE,MBC,MNB,NQL,NQLS)
    CALL ALL1
    CALL ALL2
    CALL ALL3
    CALL ALL4
    CALL ALL5
    CALL ALL6
    CALL ALL7
    CALL ALL8
    CALL ALL9
    CALL ALL10

    !WRITE (*,*) ' INPUT 0,1 FOR CONTINUING,NEW CALCULATION:- '
    !READ (*,*) NQL
    !WRITE (*,*) ' INPUT 0,1 FOR CONTINUING,NEW SAMPLE:- '
    !READ (*,*) NQLS

    IF (NQL==1) THEN

        CALL INITJET

    ELSE

        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMCJET.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) ALPI,ALPN,ALPT,APX,APY,FND,FFND,AME,AMEJ,AMR,AMRJ,BOLTZ,CB,CC,CCG,CG,CH,COL,CS,CSR,CSS,CT,CW,CWRX,CWRY,&
                 DTM,FNDJ,FNUM,FSPJ,FTMP,FUJ,FVJ,FWJ,FH,FW,IB,IC,IFCX,IFCY,IIS,IJET,IPL,IPLB,IPS,IPSB,IR,IRB,ISC,&
                 ISCG,ISG,ISP,ISPR,ISURF,IWF,LFLX,LFLY,LIMJ,LIMS,MOVT,NCOL,NCX,NCY,NIS,NM,NMB,NSCX,NSCY,NSMP,NPR,&
                 NPT,NSP,PI,PP,PPB,PR,PRB,PV,PVB,RPX,RPY,RWF,rou_bound,SELT,SEPT,SP,SPI,SPM,SPR,TIME,TIMI,TMPJ,&
				 T_bound,TSURF,VEFu,VEFv,VEFw,VFW,WJ,WSURF
        CLOSE (4)
        IF (NPR==NPT) NPR=0

    END IF

    IF (NQLS==1) CALL SAMPIJET

    DO
        NPR=NPR+1

        DO JJJ=1,NSP
            DO III=1,NIS
                TIME=TIME+DTM

                WRITE (*,99001) III,JJJ,NIS,NSP,NM,NCOL
99001           FORMAT (' DSMCJET:- Move',2I5,' of',2I5,I8,' Mols',F14.0,' Colls')

                CALL MOVEJET

                CALL INDEXM

                CALL COLLMR

            END DO

            CALL SAMPLEJET

        END DO

        WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
        OPEN (4,FILE='DSMCJET.RES',FORM='UNFORMATTED')
        WRITE (4) ALPI,ALPN,ALPT,APX,APY,FND,FFND,AME,AMEJ,AMR,AMRJ,BOLTZ,CB,CC,CCG,CG,CH,COL,CS,CSR,CSS,CT,CW,CWRX,CWRY,&
                 DTM,FNDJ,FNUM,FSPJ,FTMP,FUJ,FVJ,FWJ,FH,FW,IB,IC,IFCX,IFCY,IIS,IJET,IPL,IPLB,IPS,IPSB,IR,IRB,ISC,&
                 ISCG,ISG,ISP,ISPR,ISURF,IWF,LFLX,LFLY,LIMJ,LIMS,MOVT,NCOL,NCX,NCY,NIS,NM,NMB,NSCX,NSCY,NSMP,NPR,&
                 NPT,NSP,PI,PP,PPB,PR,PRB,PV,PVB,RPX,RPY,RWF,rou_bound,SELT,SEPT,SP,SPI,SPM,SPR,TIME,TIMI,TMPJ,&
				 T_bound,TSURF,VEFu,VEFv,VEFw,VFW,WJ,WSURF
        CLOSE (4)

        CALL OUTJET

        IF (NPR>=NPT) EXIT
    END DO
    DEALLOCATE(IPL,IPS,IR,PP,PV)
    DEALLOCATE(PR)
    DEALLOCATE(IPLB,IPSB,IRB,PPB,PVB,PRB)
    DEALLOCATE(IC,ISC,ISCG,IG,CC,CG,CCG)
    DEALLOCATE(ISP,SP,SPM)
    DEALLOCATE(ISPR,SPR,CT)
    DEALLOCATE(COL,CS,FSP)
    DEALLOCATE(CSR)
    DEALLOCATE(CSS)
    DEALLOCATE(FSPJ,AMEJ,AMRJ,AME,AMR,VEFu,VEFv,VEFw,rou_bound,T_bound)
    
END PROGRAM DSMCJET
!INITJET.FOR

SUBROUTINE INITJET

    USE MOLS2
    USE MOLSR
    USE MOLD
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE SAMPR
    USE COMP
    USE GEOMJET
    USE CONST2
    IMPLICIT NONE

    INTEGER :: K,J,L,M,N,MX,MY,IPROB,NX,NY,MM,NCOLM,NROW,NCS,NC,NCL,MC
    REAL :: REM,VMP,WMIN,YMID,A,WF,SC,tempvar
    REAL,ALLOCATABLE :: tempZ(:),tempu(:),tempv(:),tempw(:),temprou(:),tempT(:)
    REAL,EXTERNAL :: ERF,GAM,RF
    
    ALLOCATE(tempZ(231),tempu(231),tempv(231),tempw(231),temprou(231),tempT(231))

!--set constants

    PI=3.141592654
    SPI=SQRT(PI)
    BOLTZ=1.380622E-23

!--set data variables to default values that they retain if the data
!----does not reset them to specific values
    IWF=0
    FND=0.
    FFND=0.
    FTMP=273.
    VFW=0.
    IFCX=0
    IFCY=0
    LFLX=0
    LFLY=0
    ALPI(1)=-1.
    ALPI(2)=-1.
    ALPI(3)=-1.
    ALPI(4)=-1.
    WSURF(1)=0.
    WSURF(2)=0.
    WSURF(3)=0.
    WSURF(4)=0.
    NMB=0
    DO N=1,4
        IB(N)=3
        DO L=1,MNSP
            ISP(L)=1
            FSP(L)=0.
            DO K=1,MBC
                VEFu(K)=0.
                VEFv(K)=0.
                VEFw(K)=0.
                rou_bound(K)=0.
                T_bound(K)=0.
                AME(N,L,K)=0.
                AMR(N,L,K)=RF(0)
            END DO
        END DO
    END DO
    DO L=1,MNSP
        DO K=1,MBC
            AMEJ(L,K)=0.
            AMRJ(L,K)=RF(0)
        END DO
    END DO

    CALL DATAJET

!--set additional data on the gas

    IF (MNSP==1) ISPD=0
    DO N=1,MNSP
        DO M=1,MNSP
            IF ((ISPR(3,N)==0).AND.(M/=N)) THEN
                SPR(1,N,M)=SPR(1,N,N)
                SPR(2,N,M)=SPR(2,N,N)
                SPR(3,N,M)=SPR(3,N,N)
            END IF
            IF ((ISPD==0).OR.(N==M)) THEN
                SPM(1,N,M)=0.25*PI*(SP(1,N)+SP(1,M))**2
!--the collision cross section is assumed to be given by eqn (1.35)
                SPM(2,N,M)=0.5*(SP(2,N)+SP(2,M))
                SPM(3,N,M)=0.5*(SP(3,N)+SP(3,M))
                SPM(4,N,M)=0.5*(SP(4,N)+SP(4,M))
!--mean values are used for ISPD=0
            ELSE
                SPM(1,N,M)=PI*SPM(1,N,M)**2
!--the cross-collision diameter is converted to the cross-section
            END IF
            SPM(5,N,M)=(SP(5,N)/(SP(5,N)+SP(5,M)))*SP(5,M)
!--the reduced mass is defined in eqn (2.7)
            SPM(6,N,M)=GAM(2.5-SPM(3,N,M))
        END DO
    END DO

!--initialise variables

    TIME=0.
    NM=0
    NPR=0
    NCOL=0
    MOVT=0.
    SELT=0.
    SEPT=0.

    DO M=1,MNSP
        DO N=1,MNSP
            COL(M,N)=0.
        END DO
    END DO

    FW=CB(2)-CB(1)
    FH=CB(4)-CB(3)
    CG(1,1)=CB(1)
    IF (IFCX==0) THEN
        CW=FW/NCX
    ELSE
        RPX=CWRX**(1./(NCX-1.))
!--RPX is the ratio in the geometric progression
        APX=(1.-RPX)/(1.-RPX**NCX)
!--AP is the first term of the progression
    END IF
    CG(4,1)=CB(3)
    IF (IFCY==0) THEN
        CH=FH/NCY
!--CH is the uniform cell height
    ELSE
        RPY=CWRY**(1./(NCY-1.))
!--RPY is the ratio in the geometric progression
        APY=(1.-RPY)/(1.-RPY**NCY)
!--APY is the first term of the progression
    END IF
    DO MY=1,NCY
        DO MX=1,NCX
            M=(MY-1)*NCX+MX
!--M is the cell number
            CT(M)=FTMP
!--the macroscopic temperature is set to the freestream temperature
!--set the x coordinates
            IF (MX==1) CG(1,M)=CG(1,1)
            IF (MX>1) CG(1,M)=CG(2,M-1)
            IF (IFCX==0) THEN
                CG(2,M)=CG(1,M)+CW
            ELSE
                CG(2,M)=CG(1,M)+FW*APX*RPX**(MX-1)
            END IF
            CG(3,M)=CG(2,M)-CG(1,M)
!--set the y coordinates
            IF (MY==1) CG(4,M)=CG(4,1)
            IF (MY>1.AND.MX==1) CG(4,M)=CG(5,M-1)
            IF (MY>1.AND.MX>1) CG(4,M)=CG(4,M-1)
            IF (IFCY==0) THEN
                CG(5,M)=CG(4,M)+CH
            ELSE
                CG(5,M)=CG(4,M)+FH*APY*RPY**(MY-1)
            END IF
            CG(6,M)=CG(5,M)-CG(4,M)
            CC(M)=PI*CG(3,M)*(CG(5,M)**2-CG(4,M)**2)
            DO L=1,MNSG
                DO K=1,MNSG
                    CCG(2,M,L,K)=RF(0)
                    CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
                END DO
            END DO
!--the maximum value of the (rel. speed)*(cross-section) is set to a
!--reasonable, but low, initial value and will be increased as necessary
        END DO
    END DO

    IF (IFCX==1) THEN
        APX=(1.-RPX)/APX
        RPX=LOG(RPX)
!--APX and RPX are now the convenient terms in eqn (12.1)
    END IF
    IF (IFCY==1) THEN
        APY=(1.-RPY)/APY
        RPY=LOG(RPY)
!--APY and RPY are now the convenient terms in eqn (12.1)
    END IF

!--set sub-cells

    DO N=1,MNC
        DO M=1,NSCY
            DO K=1,NSCX
                L=(N-1)*NSCX*NSCY+(M-1)*NSCX+K
                ISC(L)=N
            END DO
        END DO
    END DO

    IF (IIS>0.AND.ISG==1) THEN
!--if IIS=1 generate initial gas with temperature FTMP
!
        DO L=1,MNSP
            REM=0.
            IF (IIS==1) VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
            DO N=1,MNC
                IPROB=1
                IF (IWF==0) THEN
                    WMIN=1.
                ELSE
                    WMIN=(CG(4,N)**3)/RWF
                    IF (WMIN<1.) WMIN=1.
                END IF
!--WMIN is the minimum weighting factor in the cell
                IF (LFLX/=0) THEN
                    NY=(N-1)/NCX+1
                    NX=N-(NY-1)*NCX
!--NX and NY are the cell column and row
                    IF ((LFLX>0.AND.LFLY>0).AND.(NX<LFLX.AND.NY<LFLY)) IPROB=0
                    IF ((LFLX>0.AND.LFLY<0).AND.(NX<LFLX.AND.NY>-LFLY)) IPROB=0
                    IF ((LFLX<0.AND.LFLY>0).AND.(NX>-LFLX.AND.NY<LFLY)) IPROB=0
                    IF ((LFLX<0.AND.LFLY<0).AND.(NX>-LFLX.AND.NY>-LFLY)) IPROB=0
                END IF
                IF (IPROB==1) THEN
                    A=FFND*CC(N)*FSP(L)/(FNUM*WMIN)+REM
!--A is the nunber of simulated molecules of species L in cell N to
!--simulate the required concentrations at a total number density of FND
                    IF (N<MNC) THEN
                        MM=A
                        REM=(A-MM)
!--the remainder REM is carried forward to the next cell
                    ELSE
                        MM=NINT(A)
                    END IF
                    IF (MM>0) THEN
                        DO M=1,MM
                            IF (NM<MNM) THEN
!--round-off error could have taken NM to MNM+1
                                NM=NM+1
                                IF (MNSP>1) IPS(NM)=L
                                PP(1,NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
                                NCOLM=(PP(1,NM)-CG(1,N))*(NSCX-.001)/CG(3,N)+1
!--set the random radius from the distribution of eqn (C6)
                                PP(2,NM)=(CG(4,N)**3+RF(0)*(CG(5,N)**3-CG(4,N)**3))**(1.0/3.0)
                                IF (IWF==1) THEN
                                    WF=(PP(2,NM)**3)/RWF
                                    IF (WF<1.) WF=1.
                                    IF (WMIN/WF<RF(0)) THEN
                                        NM=NM-1
                                        CYCLE
!--above takes account of the weighting factor variation in the cell
                                    END IF
                                END IF
                                IR(NM)=NM
                                NROW=(PP(2,NM)-CG(4,N))*(NSCY-.001)/CG(6,N)+1
                                IPL(NM)=(N-1)*NSCX*NSCY+(NROW-1)*NSCX+NCOLM
!--species, position, and sub-cell number have been set
                                DO K=1,3
                                    CALL RVELC(PV(K,NM),A,VMP)
                                END DO
!--velocity components have been set
!--set the rotational energy
                                IF (ISPR(1,L)>0) CALL SROT(PR(NM),FTMP,ISPR(1,L))
                            END IF
                        END DO
                    END IF
                END IF
            END DO
        END DO

        WRITE (*,99008) NM
99008   FORMAT (' ',I6,' MOLECULES')
    
    ELSE IF (ISG==2) THEN
!--if IIS=1 generate initial gas with temperature FTMP

        DO L=1,MNSP
            REM=0
            IF (IIS==1) VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
            DO N=1,MNC
                IPROB=1
                IF (IWF==0) THEN
                    WMIN=1.
                ELSE
                    WMIN=(CG(4,N)**3)/RWF
                    IF (WMIN<1.) WMIN=1.
                END IF
!--WMIN is the minimum weighting factor in the cell
                IF (LFLX/=0) THEN
                    NY=(N-1)/NCX+1
                    NX=N-(NY-1)*NCX
!--NX and NY are the cell column and row
                    IF ((LFLX>0.AND.LFLY>0).AND.(NX<LFLX.AND.NY<LFLY)) IPROB=0
                    IF ((LFLX>0.AND.LFLY<0).AND.(NX<LFLX.AND.NY>-LFLY)) IPROB=0
                    IF ((LFLX<0.AND.LFLY>0).AND.(NX>-LFLX.AND.NY<LFLY)) IPROB=0
                    IF ((LFLX<0.AND.LFLY<0).AND.(NX>-LFLX.AND.NY>-LFLY)) IPROB=0
                END IF
                IF (IPROB==1) THEN
                    YMID=0.5*(CG(4,N)+CG(5,N))
                    FFND=3.219E+24*exp(7056.37*(YMID*YMID-0.0036))
                    A=FFND*CC(N)*FSP(L)/(FNUM*WMIN)+REM
!--A is the nunber of simulated molecules of species L in cell N to
!--simulate the required concentrations at a total number density of FND
                    IF (N<MNC) THEN
                        MM=A
                        REM=(A-MM)
!--the remainder REM is carried forward to the next cell
                    ELSE
                        MM=NINT(A)
                    END IF
                    IF (MM>0) THEN
                        DO M=1,MM
                            IF (NM<MNM) THEN
!--round-off error could have taken NM to MNM+1
                                NM=NM+1
                                IF (MNSP>1) IPS(NM)=L
                                PP(1,NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
                                NCOLM=(PP(1,NM)-CG(1,N))*(NSCX-.001)/CG(3,N)+1
!--set the random radius from the distribution of eqn (C6)
                                PP(2,NM)=(CG(4,N)**3+RF(0)*(CG(5,N)**3-CG(4,N)**3))**(1.0/3.0)
                                IF (IWF==1) THEN
                                    WF=(PP(2,NM)**3)/RWF
                                    IF (WF<1.) WF=1.
                                    IF (WMIN/WF<RF(0)) THEN
                                        NM=NM-1
                                        CYCLE
!--above takes account of the weighting factor variation in the cell
                                    END IF
                                END IF
                                IR(NM)=NM
                                NROW=(PP(2,NM)-CG(4,N))*(NSCY-.001)/CG(6,N)+1
                                IPL(NM)=(N-1)*NSCX*NSCY+(NROW-1)*NSCX+NCOLM
!--species, position, and sub-cell number have been set
                                DO K=1,3
                                    CALL RVELC(PV(K,NM),A,VMP)
                                END DO
                                PV(3,NM)=PV(3,NM)+10000.*PP(2,NM)
!--velocity components have been set
!--set the rotational energy
                                IF (ISPR(1,L)>0) CALL SROT(PR(NM),FTMP,ISPR(1,L))
                            END IF
                        END DO
                    END IF
                END IF
            END DO
        END DO

        WRITE (*,99001) NM
99001   FORMAT (' ',I6,' MOLECULES')
    END IF

    IF (IIS>0) THEN

!--calculate the number of molecules that enter at each time step
!--across the four sides of the simulated region
        OPEN (6,FILE='cfdResults',STATUS='OLD',FORM='FORMATTED')
        READ (6,*) 
        DO K=1,231
            READ (6,*) tempZ(K),temprou(K),tempu(K),tempv(K),tempw(K),tempT(K)
            tempvar=tempZ(K)
            tempZ(K)=0.24-tempvar
            tempvar=tempu(K)
            tempu(K)=-tempvar
        END DO
        CLOSE (6)
        
        DO J=1,NCX
            DO K=1,230
                IF ((CG(1,J)+CG(2,J))/2.0<tempZ(K).and.(CG(1,J)+CG(2,J))/2.0 >= tempZ(K+1)) THEN
                    VEFu(J)=tempu(K)+(tempu(K+1)-tempu(K))/(tempZ(K+1)-tempZ(K))*((CG(1,J)+CG(2,J))/2.0-tempZ(K))
                    VEFv(J)=tempv(K)+(tempv(K+1)-tempv(K))/(tempZ(K+1)-tempZ(K))*((CG(1,J)+CG(2,J))/2.0-tempZ(K))
                    VEFw(J)=tempw(K)+(tempw(K+1)-tempw(K))/(tempZ(K+1)-tempZ(K))*((CG(1,J)+CG(2,J))/2.0-tempZ(K))
                    rou_bound(J)=temprou(K)+(temprou(K+1)-temprou(K))/(tempZ(K+1)-tempZ(K))*((CG(1,J)+CG(2,J))/2.0-tempZ(K))
                    T_bound(J)=tempT(K)+(tempT(K+1)-tempT(K))/(tempZ(K+1)-tempZ(K))*((CG(1,J)+CG(2,J))/2.0-tempZ(K))
                END IF
            END DO
        END DO
        
        DO N=1,4
            IF (IB(N)==1) THEN
!--molecules enter from an external stream
                DO L=1,MNSP
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
                    IF (N<3) NCS=NCY
                    IF (N>2) NCS=NCX
                    DO NC=1,NCS
                        A=1/(2.*SPI)
!--A is the non-dimensional flux of eqn (4.22)
                        IF (N==1.OR.N==2) THEN
                            VMP=SQRT(2.*BOLTZ*T_bound(NC)/SP(5,L))
                            MC=(NC-1)*NCX+1
                            AME(N,L,NC)=rou_bound(NC)/SP(5,L)*FSP(L)*A*VMP*DTM*PI*(CG(5,MC)**2-CG(4,MC)**2)/FNUM
                        ELSE
                            IF (N==3) THEN
                                VMP=SQRT(2.*BOLTZ*T_bound(NC)/SP(5,L))
                                AME(N,L,NC)=rou_bound(NC)/SP(5,L)*FSP(L)*A*VMP*DTM*2.*PI*CG(3,NC)*CB(3)/FNUM
                            END IF
                            IF (N==4) THEN
                                VMP=SQRT(2.*BOLTZ*T_bound(NC)/SP(5,L))
                                AME(N,L,NC)=rou_bound(NC)/SP(5,L)*FSP(L)*A*VMP*DTM*2.*PI*CG(3,NC)*CB(4)/FNUM
                            END IF
                        END IF
                    END DO
                END DO
            END IF
        END DO
    END IF
!--now calculate the number that enter in jet
    IF (IJET>0) THEN
!--molecules enter from a jet
        DO L=1,MNSP
            VMP=SQRT(2.*BOLTZ*TMPJ/SP(5,L))
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
            SC=FVJ/VMP
!--SC is the inward directed speed ratio
            IF (ABS(SC)<10.1) A=(EXP(-SC*SC)+SPI*SC*(1.+ERF(SC)))/(2.*SPI)
            IF (SC>10.) A=SC
            IF (SC<-10.) A=0.
!--A is the non-dimensional flux of eqn (4.22)
            NCS=LIMJ(3)-LIMJ(2)+1
            DO NC=1,NCS
                NCL=NC+LIMJ(2)-1
                IF (IJET==1.OR.IJET==2) THEN
                    MC=(LIMJ(1)-1)*NCX+1
                    WJ=2.*PI*CG(3,NCL)*CG(5,MC)
                ELSE
                    MC=(NCL-1)*NCX+1
                    WJ=PI*(CG(5,MC)**2-CG(4,MC)**2)
!--WJ is the cross-sectional area of the jet in this element
                END IF
                AMEJ(L,NC)=FNDJ*FSPJ(L)*A*VMP*DTM*WJ/FNUM
            END DO
        END DO
    END IF
    DEALLOCATE(tempZ,tempu,tempv,tempw,temprou,tempT)
END SUBROUTINE INITJET
!MOVEJET.FOR

SUBROUTINE MOVEJET

!--the NM molecules are moved over the time interval DTM

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE SAMPS
    USE COMP
    USE GEOMJET
    IMPLICIT NONE	

    INTEGER :: FLAG,N,IFT,MSC,MC,KS,L1,L2,L3,MCS,NUM,NI,MCX,MCY,MSCX,MSCY,K
    REAL :: AT,XI,YI,DX,DY,DZ,X,YD,Y,YS,A,B,C,S1,S2,S,XC,XSU,XSD,WF,XS,YCD,ZC,YC,YSU,YSD,A1,WFI,XD
    REAL,EXTERNAL :: RF

    IFT=-1
!--a negative IFT indicates that molecules have not entered at this step
    N=0
    DO
        FLAG=0
        N=N+1
        IF (N<=NM) THEN
            IF (IR(N)<0) CYCLE
!--a duplicated molecule that has already moved has a negative IR
            IF (IFT<0) AT=DTM
            IF (IFT>0) AT=RF(0)*DTM
!--the time step is a random fraction of DTM for entering molecules
            DO
                FLAG=0
                MOVT=MOVT+1
                MSC=IPL(N)
                IF (MSC<=0.OR.MSC>MNSC) CALL REMOVE(N)
                MC=ISC(MSC)
!--MC is the initial cell number
                XI=PP(1,N)
                IF ((XI+0.00001*CG(3,1))<CB(1).OR.(XI-0.00001*CG(3,MNC))>CB(2)) THEN
                    WRITE (*,*) ' MOL ',N,' X COORD OUTSIDE FLOW ',XI
                    CALL REMOVE(N)
                    FLAG=2
                    EXIT
                END IF
                YI=PP(2,N)
                IF ((YI+0.00001*CG(6,1))<CB(3).OR.(YI-0.00001*CG(6,MNC))>CB(4)) THEN
                    WRITE (*,*) ' MOL ',N,' Y COORD OUTSIDE FLOW ',YI
                    CALL REMOVE(N)
                    FLAG=2
                    EXIT
                END IF
                DX=PV(1,N)*AT
                DY=PV(2,N)*AT
                DZ=PV(3,N)*AT
                X=XI+DX
                YD=YI+DY
                Y=SQRT(YD*YD+DZ*DZ)
                DO KS=1,4
!--check the surfaces
                    IF (ISURF(KS)>0) THEN
                        IF (ISURF(KS)==1.OR.ISURF(KS)==2) THEN
                            L1=LIMS(KS,1)
                            IF (L1<=NCY) THEN
                                YS=CG(4,(L1-1)*NCX+1)
                            ELSE
                                YS=CB(4)
                                L1=L1-1
                            END IF
                            IF ((ISURF(KS)==1.AND.(YI>YS.AND.YD<YS)).OR.(ISURF(KS)==2.AND.(YI<YS.AND.Y>YS))) THEN
                                A=DY**2+DZ**2
                                B=YI*DY/A
                                C=B*B-(YI*YI-YS*YS)/A
                                IF (C>0.) THEN
                                    C=SQRT(C)
                                    S1=-B+C
                                    S2=-B-C
!--S1 and S2 are the trajectory fractions to the intersection points
!--the mol. collides with the cyl. if there is a value between 0 and 1
                                    IF (S2<0.) THEN
                                        IF (S1>0.) THEN
                                            S=S1
                                        ELSE
                                            S=2.
                                        END IF
                                    ELSE IF (S1<S2) THEN
                                        S=S1
                                    ELSE
                                        S=S2
                                    END IF
                                ELSE
                                    S=2.
!--setting S to 2 indicates that there is no intersection
                                END IF
!--S is the least positive solution
                                IF (S<1.) THEN
                                    XC=XI+S*DX
                                    IF (XC<=CB(1).AND.IB(1)==2) THEN
                                        XC=2.*CB(1)-XC
                                        PV(1,N)=-PV(1,N)
                                    END IF
                                    IF (XC>=CB(2).AND.IB(2)==2) THEN
                                        XC=2.*CB(2)-XC
                                        PV(1,N)=-PV(1,N)
                                    END IF
                                    L2=LIMS(KS,2)
                                    L3=LIMS(KS,3)
                                    XSU=CG(1,L2)
                                    XSD=CG(2,L3)
                                    IF (XC>XSU.AND.XC<XSD) THEN
!--molecule collides with surface at XC
                                        IF (IFCX==0) THEN
                                            MC=(XC-CB(1))/CW+0.99999
                                        ELSE
                                            XD=(XC-CB(1))/FW+1.E-6
                                            MC=1.+(LOG(1.-XD*APX))/RPX
!--the cell number is calculated from eqn (12.1)
                                        END IF
                                        IF (MC<1) MC=1
                                        IF (MC>NCX) MC=NCX
                                        MCS=MC-(L2-1)
                                        IF (ISURF(KS)==1) MC=MC+(L1-1)*NCX
                                        IF (ISURF(KS)==2) MC=MC+(L1-2)*NCX
!--MC is the cell number for the reflected molecule
                                        IF (KS>1) THEN
                                            DO NUM=2,KS
                                                MCS=MCS+LIMS(NUM-1,3)-LIMS(NUM-1,2)+1
!--MCS is the code number of the surface element
                                            END DO
                                        END IF
                                        AT=AT*(X-XC)/DX
                                        CALL AIFR(YI,DY*S,DZ*S,YS,PV(2,N),PV(3,N))
                                        CALL REFLECTJET(N,KS,MCS,XC,YS,MC,YI)
                                        IF (IWF==1) THEN
                                            IF ((YI**3)>RWF.OR.(YS**3)>RWF) THEN
                                                WFI=(YI**3)/RWF
                                                IF (WFI<1.) WFI=1.
                                                WF=(YS**3)/RWF
                                                IF (WF<1.) WF=1.
                                                NI=N
                                                CALL WEIGHT(N,WFI,WF)
                                                IF (N<NI) THEN
                                                    FLAG=2
                                                    EXIT
                                                END IF
                                            END IF
                                        END IF
                                        FLAG=1
                                        EXIT
                                    END IF
                                END IF
                            END IF
                        END IF
                        IF (ISURF(KS)==3.OR.ISURF(KS)==4) THEN
                            L1=LIMS(KS,1)
                            IF (L1<=NCX) THEN
                                IF (ISURF(KS)==3) THEN
                                    XS=CG(1,L1)
                                ELSE
                                    XS=CG(2,L1)
                                END IF
                            ELSE
                                XS=CB(2)
                                L1=L1-1
                            END IF
                            IF ((ISURF(KS)==3.AND.(XI>XS.AND.X<XS)).OR.(ISURF(KS)==4.AND.(XI<XS.AND.X>XS))) THEN
                                YCD=YI+(XS-XI)*DY/DX
                                ZC=(XS-XI)*DZ/DX
                                YC=SQRT(YCD**2+ZC**2)
                                L2=LIMS(KS,2)
                                L3=LIMS(KS,3)
                                YSU=CG(4,(L2-1)*NCX+1)
                                YSD=CG(5,(L3-1)*NCX+1)
                                IF (YC>YSU.AND.YC<YSD) THEN
!--molecule collides with surface at YC
                                    IF (IFCY==0) THEN
                                        MC=(YC-CB(3))/CH+0.99999
                                    ELSE
                                        YD=(YC-CB(3))/FH+1.E-6
                                        MC=1.+(LOG(1.-YD*APY))/RPY
!--the cell number is calculated from eqn (12.1)
                                    END IF
                                    IF (MC<1) MC=1
                                    IF (MC>NCY) MC=NCY
                                    MCS=MC-(L2-1)
                                    IF (ISURF(KS)==3) MC=(MC-1)*NCX+L1
                                    IF (ISURF(KS)==4) MC=(MC-1)*NCX+L1-1
!--MC is the cell number for the reflected molecule
                                    IF (KS>1) THEN
                                        DO NUM=2,KS
                                            MCS=MCS+LIMS(NUM-1,3)-LIMS(NUM-1,2)+1
!--MCS is the code number of the surface element
                                        END DO
                                    END IF
                                    A1=AT*(XS-XI)/DX
                                    AT=AT*(X-XS)/DX
                                    CALL AIFR(YI,PV(2,N)*A1,PV(3,N)*A1,YC,PV(2,N),PV(3,N))
                                    CALL REFLECTJET(N,KS,MCS,XS,YC,MC,YI)
                                    IF (IWF==1) THEN
                                        IF ((YI**3)>RWF.OR.(YC**3)>RWF) THEN
                                            WFI=(YI**3)/RWF
                                            IF (WFI<1.) WFI=1.
                                            WF=(YC**3)/RWF
                                            IF (WF<1.) WF=1.
                                            NI=N
                                            CALL WEIGHT(N,WFI,WF)
                                            IF (N<NI) THEN
                                                FLAG=2
                                                EXIT
                                            END IF
                                        END IF
                                    END IF
                                    FLAG=1
                                    EXIT
                                END IF
                            END IF
                        END IF
                    END IF
                END DO
                IF (FLAG/=1) EXIT
            END DO
            IF (FLAG==2) CYCLE
            CALL AIFR(YI,DY,DZ,Y,PV(2,N),PV(3,N))
            IF (X<CB(1).OR.X>CB(2)) THEN
                IF (X<CB(1)) K=1
                IF (X>CB(2)) K=2
!--intersection with boundary K
                IF (IB(K)==2) THEN
!--specular reflection from the boundary (eqn (11.7))
                    X=2.*CB(K)-X
                    PV(1,N)=-PV(1,N)
                ELSE
!--molecule leaves flow
                    CALL REMOVE(N)
                    CYCLE
                END IF
            END IF
            IF (Y<CB(3).OR.Y>CB(4)) THEN
                IF (Y<CB(3)) K=3
                IF (Y>CB(4)) K=4
!--intersection with boundary K
                IF (IB(K)==2) THEN
!--specular reflection from the boundary is not allowed
                    WRITE (*,*) ' CURVED BOUNDARIES CANNOT BE A PLANE OF SYMMETRY'
                    STOP
                ELSE
!--molecule leaves flow
                    CALL REMOVE(N)
                    CYCLE
                END IF
            END IF

            IF (X<CG(1,MC).OR.X>CG(2,MC).OR.Y<CG(4,MC).OR.Y>CG(5,MC)) THEN
!--the molecule has moved from the initial cell
                IF (IFCX==0) THEN
                    MCX=(X-CB(1))/CW+0.99999
                ELSE
                    XD=(X-CB(1))/FW+1.E-6
                    MCX=1.+(LOG(1.-XD*APX))/RPX
!--the cell number is calculated from eqn (12.1)
                END IF
                IF (MCX<1) MCX=1
                IF (MCX>NCX) MCX=NCX
!--MCX is the new cell column (note avoidance of round-off error)
                IF (IFCY==0) THEN
                    MCY=(Y-CB(3))/CH+0.99999
                ELSE
                    YD=(Y-CB(3))/FH+1.E-6
                    MCY=1.+(LOG(1.-YD*APY))/RPY
!--the cell number is calculated from eqn (12.1)
                END IF
                IF (MCY<1) MCY=1
                IF (MCY>NCY) MCY=NCY
!--MCY is the new cell row (note avoidance of round-off error)
                MC=(MCY-1)*NCX+MCX
            END IF
            MSCX=((X-CG(1,MC))/CG(3,MC))*(NSCX-.001)+1
            MSCY=((Y-CG(4,MC))/CG(6,MC))*(NSCY-.001)+1
            MSC=(MSCY-1)*NSCX+MSCX+NSCX*NSCY*(MC-1)
!--MSC is the new sub-cell number
            IF (MSC<1) MSC=1
            IF (MSC>MNSC) MSC=MNSC
            IPL(N)=MSC
            PP(1,N)=X
            PP(2,N)=Y
            IF (IWF==1) THEN
                IF ((YI**3)>RWF.OR.(Y**3)>RWF) THEN
                    WFI=(YI**3)/RWF
                    IF (WFI<1.) WFI=1.
                    WF=(Y**3)/RWF
                    IF (WF<1.) WF=1.
                    CALL WEIGHT(N,WFI,WF)
                END IF
            END IF
            CYCLE
        ELSE IF (IFT<0) THEN
            IFT=1
!--new molecules enter
            CALL ENTERJET
            N=N-1
            CYCLE
        END IF
        EXIT
    END DO
END SUBROUTINE MOVEJET
!ENTERJET.FOR

SUBROUTINE ENTERJET

!--new molecules enter at boundaries

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE COMP
    USE GEOMJET
    USE CONST2
    IMPLICIT NONE

    INTEGER :: N,NCS,NC,L,M,K,MC,MSCX,MSCY,MSC,NCL
    REAL :: WMIN,VMP,A,WF,SC,FS1,FS2,QA,U,UN,XJ,YJ
    REAL,EXTERNAL :: RF

    DO N=1,4
!--consider each boundary in turn
        IF (IB(N)==1) THEN
            IF (N<3) NCS=NCY
            IF (N>2) NCS=NCX
            DO NC=1,NCS
                IF (IWF==0) THEN
                    WMIN=1.
                ELSE IF (N<3) THEN
                    MC=(NC-1)*NCX+1
                    WMIN=(CG(4,MC)**3)/RWF
                    IF (WMIN<1.) WMIN=1.
                ELSE
                    WMIN=(CB(N)**3)/RWF
                    IF (WMIN<1.) WMIN=1.
                END IF
                IF (LFLX/=0) THEN
!--bypass entry into the excluded region of the flow
                    IF (N==1) THEN
                        IF (LFLY>0.AND.LFLX>0.AND.NC<LFLY) CYCLE
                        IF (LFLY<0.AND.LFLX>0.AND.NC>LFLY) CYCLE
                    END IF
                    IF (N==2) THEN
                        IF (LFLY>0.AND.LFLX<0.AND.NC<LFLY) CYCLE
                        IF (LFLY<0.AND.LFLX<0.AND.NC>LFLY) CYCLE
                    END IF
                    IF (N==3) THEN
                        IF (LFLX>0.AND.LFLY>0.AND.NC<LFLX) CYCLE
                        IF (LFLX<0.AND.LFLY>0.AND.NC>LFLX) CYCLE
                    END IF
                    IF (N==4) THEN
                        IF (LFLX>0.AND.LFLY<0.AND.NC<LFLX) CYCLE
                        IF (LFLX<0.AND.LFLY<0.AND.NC>LFLX) CYCLE
                    END IF
                END IF
                DO L=1,MNSP
!--consider each species in turn
                    VMP=SQRT(2.*BOLTZ*T_bound(NC)/SP(5,L))
                    A=AME(N,L,NC)/WMIN+AMR(N,L,NC)
                    M=A
                    AMR(N,L,NC)=A-M
!--M molecules enter, remainder has been reset
                    IF (M>0) THEN
                        DO K=1,M
                            IF (NM<MNM) THEN
                                NM=NM+1
!--NM is now the number of the new molecule
                                IF (N==1) PV(1,NM)=SQRT(-LOG(RF(0)))*VMP
                                IF (N==2) PV(1,NM)=-SQRT(-LOG(RF(0)))*VMP
                                IF (N==3) PV(2,NM)=SQRT(-LOG(RF(0)))*VMP
                                IF (N==4) PV(2,NM)=-SQRT(-LOG(RF(0)))*VMP
!--for a stationary external gas, use eqn (12.3)
                                IF (N<3) CALL RVELC(PV(2,NM),PV(3,NM),VMP)
                                IF (N>2) THEN
                                    CALL RVELC(PV(1,NM),PV(3,NM),VMP)
                                    PV(1,NM)=PV(1,NM)+VEFu(NC)
                                    PV(2,NM)=PV(2,NM)+VEFv(NC)
                                    PV(3,NM)=PV(3,NM)+VEFw(NC)
                                END IF
!--a single call of RVELC generates the two normal velocity components
                                IF (ISPR(1,L)>0) CALL SROT(PR(NM),T_bound(NC),ISPR(1,L))
                                IF (N==1) PP(1,NM)=CB(1)+0.001*CG(3,1)
                                IF (N==2) PP(1,NM)=CB(2)-0.001*CG(3,MNC)
                                IF (N==3) PP(2,NM)=CB(3)+0.001*CG(6,1)
                                IF (N==4) PP(2,NM)=CB(4)-0.001*CG(6,MNC)
!--the molecule is moved just off the boundary
                                IF (MNSP>1) IPS(NM)=L
                                IF (N<3) THEN
                                    IF (N==1) MC=(NC-1)*NCX+1
                                    IF (N==2) MC=NC*NCX
                                    PP(2,NM)=(CG(4,MC)**3+RF(0)*(CG(5,MC)**3-CG(4,MC)**3))**(1.0/3.0)
!--this case employs eqn (C6) for the selection of radius
                                END IF
                                IF (N>2) THEN
                                    IF (N==3) MC=NC
                                    IF (N==4) MC=(NCY-1)*NCX+NC
                                    PP(1,NM)=CG(1,MC)+RF(0)*CG(3,MC)
                                END IF
                                IR(NM)=NM
                                IF (IWF==1) THEN
                                    WF=(PP(2,NM)**3)/RWF
                                    IF (WF<1.) WF=1.
                                    IF (WMIN/WF<RF(0)) THEN
                                        NM=NM-1
                                        CYCLE
!--above takes account of the weighting factor variation in the cell
                                    END IF
                                END IF
                                MSCX=((PP(1,NM)-CG(1,MC))/CG(3,MC))*(NSCX-.001)+1
                                MSCY=((PP(2,NM)-CG(4,MC))/CG(6,MC))*(NSCY-.001)+1
                                MSC=(MSCY-1)*NSCX+MSCX+NSCX*NSCY*(MC-1)
!--MSC is the new sub-cell number
                                IF (MSC<1) MSC=1
                                IF (MSC>MNSC) MSC=MNSC
                                IPL(NM)=MSC
                            ELSE
                                WRITE (*,*) ' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
                            END IF
                        END DO
                    END IF
                END DO
            END DO
        END IF
    END DO
!--now the jet molecules
    IF (IJET>0) THEN
        NCS=LIMJ(3)-LIMJ(2)+1
        DO NC=1,NCS
            DO L=1,MNSP
!--consider each species in turn
                VMP=SQRT(2.*BOLTZ*TMPJ/SP(5,L))
                NCL=NC+LIMJ(2)-1
                IF (IWF==1) THEN
                    WMIN=(CG(4,NCL)**3)/RWF
                    IF (WMIN<1.) WMIN=1.
                ELSE
                    WMIN=1.
                END IF
                A=AMEJ(L,NC)/WMIN+AMRJ(L,NC)
                M=A
                AMRJ(L,NC)=A-M
!--M molecules enter, remainder has been reset
                IF (M>0) THEN
                    IF (ABS(FVJ)>1.E-6) SC=FVJ/VMP
                    FS1=SC+SQRT(SC*SC+2.)
                    FS2=0.5*(1.+SC*(2.*SC-FS1))
!the above constants are required for the entering distn. of eqn (12.5)
                    DO K=1,M
                        IF (NM<MNM) THEN
                            NM=NM+1
!--NM is now the number of the new molecule
                            IF (ABS(FVJ)>1.E-6) THEN
                                QA=3.
                                IF (SC<-3.) QA=ABS(SC)+1.
                                DO
                                    U=-QA+2.*QA*RF(0)
!--U is a potential normalised thermal velocity component
                                    UN=U+SC
!--UN is a potential inward velocity component
                                    IF (UN<0.) CYCLE
                                    A=(2.*UN/FS1)*EXP(FS2-U*U)
                                    IF (A<RF(0)) CYCLE
                                    EXIT
                                END DO
!--the inward normalised vel. component has been selected (eqn (12.5))
                                IF (IJET==1) PV(2,NM)=UN*VMP
                                IF (IJET==2) PV(2,NM)=-UN*VMP
                                IF (IJET==3) PV(1,NM)=UN*VMP
                                IF (IJET==4) PV(1,NM)=-UN*VMP
                            ELSE
                                IF (IJET==1) PV(2,NM)=SQRT(-LOG(RF(0)))*VMP
                                IF (IJET==2) PV(2,NM)=-SQRT(-LOG(RF(0)))*VMP
                                IF (IJET==3) PV(1,NM)=SQRT(-LOG(RF(0)))*VMP
                                IF (IJET==4) PV(1,NM)=-SQRT(-LOG(RF(0)))*VMP
!--for a stationary external gas, use eqn (12.3)
                            END IF
                            IF (IJET<3) THEN
                                CALL RVELC(PV(1,NM),PV(3,NM),VMP)
                                PV(1,NM)=PV(1,NM)+FUJ;
                                PV(3,NM)=PV(3,NM)+FWJ;
                            END IF
                            IF (IJET>2) THEN
                                CALL RVELC(PV(2,NM),PV(3,NM),VMP)
                                PV(2,NM)=PV(2,NM)+FVJ;
                                PV(3,NM)=PV(3,NM)+FWJ;
                            END IF
!--a single call of RVELC generates the two normal velocity components
                            IF (ISPR(1,L)>0) CALL SROT(PR(NM),TMPJ,ISPR(1,L))
                            IF (IJET<3) THEN
                                MC=(LIMJ(1)-1)*NCX+LIMJ(2)-1+NC
                                YJ=CG(4,MC)
                                IF (IJET==2) MC=MC-NCX
                            END IF
                            IF (IJET>2) THEN
                                MC=LIMJ(1)+(LIMJ(2)-1)*NCX+(NC-1)*NCX
                                XJ=CG(1,MC)
                                IF (IJET==4) MC=MC-1
                            END IF
                            IF (IJET==1) PP(2,NM)=YJ+0.001*CG(6,MC)
                            IF (IJET==2) PP(2,NM)=YJ-0.001*CG(6,MC)
                            IF (IJET==3) PP(1,NM)=XJ+0.001*CG(3,MC)
                            IF (IJET==4) PP(1,NM)=XJ-0.001*CG(3,MC)
!--the molecule is moved just off the boundary
                            IF (MNSP>1) IPS(NM)=L
                            IR(NM)=NM
                            IF (IJET<3) PP(1,NM)=CG(1,MC)+RF(0)*CG(3,MC)
                            IF (IJET>2) PP(2,NM)=(CG(4,N)**3+RF(0)*(CG(5,N)**3-CG(4,N)**3))**(1.0/3.0)
                            IF (IWF==1) THEN
                                WF=(PP(2,NM)**3)/RWF
                                IF (WF<1.) WF=1.
                                IF (WMIN/WF<RF(0)) THEN
                                    NM=NM-1
                                    CYCLE
!--above takes account of the weighting factor variation in the cell
                                END IF
                            END IF
                            MSCX=((PP(1,NM)-CG(1,MC))/CG(3,MC))*(NSCX-.001)+1
                            MSCY=((PP(2,NM)-CG(4,MC))/CG(6,MC))*(NSCY-.001)+1
                            MSC=(MSCY-1)*NSCX+MSCX+NSCX*NSCY*(MC-1)
!--MSC is the new sub-cell number
                            IF (MSC<1) MSC=1
                            IF (MSC>MNSC) MSC=MNSC
                            IPL(NM)=MSC
                        ELSE
                            WRITE (*,*) ' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
                        END IF
                    END DO
                END IF
            END DO
        END DO
    END IF
END SUBROUTINE ENTERJET
!REFLECTJET.FOR
SUBROUTINE REFLECTJET(N,KS,K,XC,YC,MC,YI)

!--reflection of molecule N from surface KS, element K,
!----location XC,YC, cell MC

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE SAMPS
    USE COMP
    USE GEOMJET
    USE CONST2
    IMPLICIT NONE

    INTEGER :: N,KS,K,MC
    REAL :: XC,YC,YI
    INTEGER :: L
    REAL :: WF,VMP,VNI,VPI,UPI,WPI,ANG,ALPHAN,R,TH,UM,VN,ALPHAT,VP,WP,ALPHAI,OM,CTH,X,A
    REAL,EXTERNAL :: RF

    IF (IWF==1.AND.(YI**3)>RWF) THEN
        WF=(YI**3)/RWF
    ELSE
        WF=1.
    END IF
    IF (MNSP>1) THEN
        L=IPS(N)
    ELSE
        L=1
    END IF
!--sample the surface properies due to the incident molecules
    CSS(1,K,L)=CSS(1,K,L)+WF
    IF (ISURF(KS)==1) THEN
        CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*WF*PV(2,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*WF*PV(1,N)
    END IF
    IF (ISURF(KS)==2) THEN
        CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*WF*PV(2,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*WF*PV(1,N)
    END IF
    IF (ISURF(KS)==3) THEN
        CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*WF*PV(1,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*WF*PV(2,N)
    END IF
    IF (ISURF(KS)==4) THEN
        CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*WF*PV(1,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*WF*PV(2,N)
    END IF
    CSS(5,K,L)=CSS(5,K,L)+0.5*SP(5,L)*WF*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
    IF (MNMR>1) CSS(7,K,L)=CSS(7,K,L)+PR(N)*WF

    IF (TSURF(KS)<0.) THEN
!--specular reflection
        IF (ISURF(KS)==1.OR.ISURF(KS)==2) PV(2,N)=-PV(2,N)
        IF (ISURF(KS)==3.OR.ISURF(KS)==4) PV(1,N)=-PV(1,N)
    ELSE IF (ALPI(KS)<0.) THEN
!--diffuse reflection
        VMP=SQRT(2.*BOLTZ*TSURF(KS)/SP(5,L))
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        IF (ISURF(KS)==1) THEN
            PV(2,N)=SQRT(-LOG(RF(0)))*VMP
            CALL RVELC(PV(1,N),PV(3,N),VMP)
            PV(3,N)=PV(3,N)+WSURF(KS)
        END IF
        IF (ISURF(KS)==2) THEN
            PV(2,N)=-SQRT(-LOG(RF(0)))*VMP
            CALL RVELC(PV(1,N),PV(3,N),VMP)
            PV(3,N)=PV(3,N)+WSURF(KS)
        END IF
        IF (ISURF(KS)==3) THEN
            PV(1,N)=SQRT(-LOG(RF(0)))*VMP
            CALL RVELC(PV(2,N),PV(3,N),VMP)
            PV(3,N)=PV(3,N)+WSURF(KS)*PP(2,N)
        END IF
        IF (ISURF(KS)==4) THEN
            PV(1,N)=-SQRT(-LOG(RF(0)))*VMP
            CALL RVELC(PV(2,N),PV(3,N),VMP)
            PV(3,N)=PV(3,N)+WSURF(KS)*PP(2,N)
        END IF
!--the normal velocity component has been generated
!--a single call of RVELC generates the two tangential vel. components
        IF (ISPR(1,L)>0) CALL SROT(PR(N),TSURF(KS),ISPR(1,L))
    ELSE IF (ALPI(KS)>=0) THEN
!--Cercignani-Lampis-Lord reflection model
        VMP=SQRT(2.*BOLTZ*TSURF(KS)/SP(5,L))
!--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        IF (ISURF(KS)==1.OR.ISURF(KS)==2) THEN
            IF (ISURF(KS)==1) VNI=-PV(2,N)/VMP
            IF (ISURF(KS)==2) VNI=PV(2,N)/VMP
            UPI=PV(1,N)/VMP
        END IF
        IF (ISURF(KS)==3.OR.ISURF(KS)==4) THEN
            IF (ISURF(KS)==3) VNI=-PV(1,N)/VMP
            IF (ISURF(KS)==4) VNI=PV(1,N)/VMP
            UPI=PV(2,N)/VMP
        END IF
        WPI=PV(3,N)/VMP
        ANG=ATAN2(WPI,UPI)
        VPI=SQRT(UPI*UPI+WPI*WPI)
!--VNI is the normalized incident normal vel. component (always +ve)
!--VPI is the normalized incident tangential vel. comp. in int. plane
!--ANG is the angle between the interaction plane and the x or y axis

!--first the normal component
        ALPHAN=ALPN(KS)
        R=SQRT(-ALPHAN*LOG(RF(0)))
        TH=2.*PI*RF(0)
        UM=SQRT(1.-ALPHAN)*VNI
        VN=SQRT(R*R+UM*UM+2.*R*UM*COS(TH))
!--VN is the normalized magnitude of the reflected normal vel. comp.
!----from eqns (14.3)

!--then the tangential component
        ALPHAT=ALPT(KS)*(2.-ALPT(KS))
        R=SQRT(-ALPHAT*LOG(RF(0)))
        TH=2.*PI*RF(0)
        UM=SQRT(1.-ALPHAT)*VPI
        VP=UM+R*COS(TH)
        WP=R*SIN(TH)
!--VP,WP are the normalized reflected tangential vel. components in and
!----normal to the interaction plane, from eqn(14.4) and (14.5)
        IF (ISURF(KS)==1.OR.ISURF(KS)==2) THEN
            IF (ISURF(KS)==1) PV(2,N)=VN*VMP
            IF (ISURF(KS)==2) PV(2,N)=-VN*VMP
            PV(1,N)=(VP*COS(ANG)-WP*SIN(ANG))*VMP
        END IF
        IF (ISURF(KS)==3.OR.ISURF(KS)==4) THEN
            IF (ISURF(KS)==3) PV(1,N)=VN*VMP
            IF (ISURF(KS)==4) PV(1,N)=-VN*VMP
            PV(2,N)=(VP*COS(ANG)-WP*SIN(ANG))*VMP
        END IF
        PV(3,N)=(VP*SIN(ANG)+WP*COS(ANG))*VMP
        IF (ISPR(1,L)>0) THEN
!--set rotational energy by analogy with normal vel. component
            ALPHAI=ALPI(KS)
            OM=SQRT(PR(N)*(1.-ALPHAI)/(BOLTZ*TSURF(KS)))
            IF (ISPR(1,L)==2) THEN
                R=SQRT(-ALPHAI*LOG(RF(0)))
                CTH=COS(2.*PI*RF(0))
            ELSE
!--for polyatomic case, apply acceptance-rejection based on eqn (14.6)
                DO
                    X=4.*RF(0)
                    A=2.7182818*X*X*EXP(-X*X)
                    IF (A<RF(0)) CYCLE
                    EXIT
                END DO
                R=SQRT(ALPHAI)*X
                CTH=2.*RF(0)-1.
            END IF
            PR(N)=BOLTZ*TSURF(KS)*(R*R+OM*OM+2.*R*OM*CTH)
        END IF
    END IF
    IF (ISURF(KS)==1) THEN
        PP(1,N)=XC
        PP(2,N)=YC+0.001*CG(6,MC)
    END IF
    IF (ISURF(KS)==2) THEN
        PP(1,N)=XC
        PP(2,N)=YC-0.001*CG(6,MC)
    END IF
    IF (ISURF(KS)==3) THEN
        PP(1,N)=XC+0.001*CG(3,MC)
        PP(2,N)=YC
    END IF
    IF (ISURF(KS)==4) THEN
        PP(1,N)=XC-0.001*CG(3,MC)
        PP(2,N)=YC
    END IF
    IPL(N)=(MC-1)*NSCX*NSCY+1
!--sample the surface properties due to the reflected molecules
    IF (ISURF(KS)==1) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*WF*PV(2,N)
    IF (ISURF(KS)==2) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*WF*PV(2,N)
    IF (ISURF(KS)==3) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*WF*PV(1,N)
    IF (ISURF(KS)==4) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*WF*PV(1,N)
    IF (ISURF(KS)==1) CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*WF*PV(1,N)
    IF (ISURF(KS)==2) CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*WF*PV(1,N)
    IF (ISURF(KS)==3) CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*WF*PV(2,N)
    IF (ISURF(KS)==4) CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*WF*PV(2,N)
    CSS(6,K,L)=CSS(6,K,L)-0.5*SP(5,L)*WF*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
    IF (MNMR>1) CSS(8,K,L)=CSS(8,K,L)-WF*PR(N)
END SUBROUTINE REFLECTJET
!REMOVE.FOR
SUBROUTINE REMOVE(N)

!--remove molecule N and replace it by molecule NM

    USE MOLS2
    USE MOLSR
    IMPLICIT NONE
    INTEGER :: N,M

    PP(1,N)=PP(1,NM)
    PP(2,N)=PP(2,NM)
    DO M=1,3
        PV(M,N)=PV(M,NM)
    END DO
    IF (MNMR>1) PR(N)=PR(NM)
    IPL(N)=IPL(NM)
    IF (MNSP>1) IPS(N)=IPS(NM)
    NM=NM-1
    N=N-1
END SUBROUTINE REMOVE
!WEIGHT.FOR

SUBROUTINE WEIGHT(N,WFI,WF)

!--weighting action for molecule N with weighting factor change

    USE MOLS2
    USE MOLSR
    USE MOLD
    IMPLICIT NONE
    INTEGER :: N
    REAL :: WFI,WF
    INTEGER :: LL,J,NTM,M
    REAL :: A
    REAL,EXTERNAL :: RF

    A=WFI/WF
    LL=0
    DO
        IF (A<1.) THEN
            IF (RF(0)<A) LL=LL+1
            IF (LL==0) CALL REMOVE(N)
            LL=LL-1
            IF (LL/=0) THEN
                DO J=1,LL
                    IF (NM<MNM) THEN
                        NTM=NMB+1
                        IF (NMB>=MNB) THEN
                            NTM=RF(0)*(MNB-.01)+1
!--a random molecule is read from the duplication delay file
                            NMB=NMB-1
                            NM=NM+1
                            DO M=1,2
                                PP(M,NM)=PPB(M,NTM)
                            END DO
                            DO M=1,3
                                PV(M,NM)=PVB(M,NTM)
                            END DO
                            IF (MNMR>1) PR(NM)=PRB(NTM)
                            IPL(NM)=IPLB(NTM)
                            IF (MNSP>1) IPS(NM)=IPSB(NTM)
                            IR(NM)=IRB(NTM)
                        END IF
!--a negative IR(NM) flags a duplicated molecule that has already moved
                        IF (NMB<MNB) NMB=NMB+1
                        DO M=1,2
                            PPB(M,NTM)=PP(M,N)
                        END DO
                        DO M=1,3
                            PVB(M,NTM)=PV(M,N)
                        END DO
                        IF (MNMR>1) PRB(NTM)=PR(N)
                        IPLB(NTM)=IPL(N)
                        IF (MNSP>1) IPSB(NTM)=IPS(N)
                        IRB(NTM)=IR(N)
                    END IF
                END DO
            END IF
            EXIT
        ELSE
            LL=LL+1
            A=A-1.
        END IF
    END DO
END SUBROUTINE WEIGHT
!SAMPIJET.FOR

SUBROUTINE SAMPIJET

!--initialises all the sampling variables

    USE SAMPJET
    USE SAMPR
    USE SAMPS
    USE COMP
    IMPLICIT NONE

    INTEGER :: L,N,M

    NSMP=0
    TIMI=TIME
    DO L=1,MNSP
        DO N=1,MNC
            CS(1,N,L)=1.E-6
            DO M=2,9
                CS(M,N,L)=0.
            END DO
            CSR(N,L)=0.
        END DO
        DO N=1,MNSE
            CSS(1,N,L)=1.E-6
            DO M=2,9
                CSS(M,N,L)=0.
            END DO
        END DO
    END DO
END SUBROUTINE SAMPIJET
!SAMPLEJET.FOR

SUBROUTINE SAMPLEJET

!--sample the molecules in the flow.

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE SAMPJET
    USE SAMPR
    USE GEOMJET
    USE COMP
    IMPLICIT NONE

    INTEGER :: NN,N,L,J,K,M,I,LL
    REAL :: WF

    NSMP=NSMP+1
    DO NN=1,MNSG
        DO N=1,MNC
            L=IC(2,N,NN)
            CS(9,N,1)=CS(9,N,1)+L*L
            IF (L>0) THEN
                DO J=1,L
                    K=IC(1,N,NN)+J
                    M=IR(K)
                    IF (IWF==1.AND.(PP(2,M)**3)>RWF) THEN
                        WF=(PP(2,M)**3)/RWF
                    ELSE
                        WF=1.
                    END IF
                    IF (MNSP>1) THEN
                        I=IPS(M)
                    ELSE
                        I=1
                    END IF
                    CS(1,N,I)=CS(1,N,I)+WF
                    CS(8,N,I)=CS(8,N,I)+1.
                    DO LL=1,3
                        CS(LL+1,N,I)=CS(LL+1,N,I)+PV(LL,M)*WF
                        CS(LL+4,N,I)=CS(LL+4,N,I)+PV(LL,M)**2*WF
                    END DO
                    IF (MNMR>1) CSR(N,I)=CSR(N,I)+WF*PR(M)
                END DO
            END IF
        END DO
    END DO
END SUBROUTINE SAMPLEJET
!OUTJET.FOR

SUBROUTINE OUTJET

!--output a progressive set of results to file DSMCJET.OUT.

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE GAS
    USE GASR
    USE SAMPJET
    USE SAMPR
    USE SAMPS
    USE GEOMJET
    USE COMP
    USE CONST2
    IMPLICIT NONE

    REAL :: VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,TROT,DBOLTZ,SS(9),MACH
    INTEGER :: KS,NEL,NEL1,NEL2,K,NC,L,M,N,RBEGIN1,RBEGIN2
    REAL :: A,X,Y,AR,FLM,SUU,DENN,DEN,UU,TEMP,XC,YC,TTX,TTY,TTZ
	
    DBOLTZ=BOLTZ

    OPEN (4,FILE='DSMCJET.OUT',FORM='FORMATTED')
    OPEN (5,FILE='dsmc_tec.DAT',FORM='FORMATTED')
    OPEN (7,FILE='dsmcResults1',FORM='FORMATTED')
    OPEN (8,FILE='dsmcResults2',FORM='FORMATTED')

    WRITE (4,*) ' FLOW SAMPLED FROM TIME ',TIMI,' TO TIME ',TIME
    WRITE (4,*) ' COLLISIONS:-'
    WRITE (4,99001) ((COL(M,L),M=1,MNSP),L=1,MNSP)
99001   FORMAT (5F13.0)
    WRITE (4,*) ' TOTAL NUMBER OF SAMPLES ',NSMP
    WRITE (4,*) NM,' MOLECULES'
    WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
    WRITE (5,*) 'TITLE     = "DSMC"'
    WRITE (5,*) 'VARIABLES = "z","r","DENSITY","TR TEMP","ROT TEMP","OVTEMP","U","V","W","MACH"'
    WRITE (5,*) 'ZONE T="Zone 1"'
    WRITE (5,*) 'I=',NCX,'J=',NCY,'K=',1,'ZONETYPE=Ordered'
    WRITE (5,*) 'DATAPACKING=POINT'
    WRITE (7,*) NCX,CB(1),CB(2)
    WRITE (8,*) NCX,CB(1),CB(2)
    IF (NCOL>0) THEN
        WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),' COLLISION EVENTS, RATIO  ',REAL(NCOL/SELT)
        IF (NCOL>0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',REAL(SEPT/NCOL)
    END IF

    WRITE (4,*)
    DO KS=1,4
        IF (ISURF(KS)>0) THEN
            WRITE (4,*) ' SURFACE ',KS
            WRITE (4,*)
            NEL=LIMS(KS,3)-LIMS(KS,2)+1
            IF (KS==1) THEN
                NEL1=1
                NEL2=NEL
            ELSE
                NEL1=NEL2+1
                NEL2=NEL1+NEL-1
            END IF
            A=FNUM/(TIME-TIMI)
            WRITE (4,*) '   X COORD     Y COORD    SAMPLE    FRACTION SPECIES 1   FRACTION SPECIES 2....'
            DO K=NEL1,NEL2
                IF (ISURF(KS)<3) THEN
                    IF (LIMS(KS,1)<=NCY) THEN
                        NC=(LIMS(KS,1)-1)*NCX+1
                        Y=CG(4,NC)
                    ELSE
                        Y=CB(4)
                    END IF
                    NC=LIMS(KS,2)+K-NEL1
                    X=0.5*(CG(1,NC)+CG(2,NC))
                END IF
                IF (ISURF(KS)>2) THEN
                    IF (LIMS(KS,1)<=NCX) THEN
                        X=CG(1,LIMS(KS,1))
                    ELSE
                        X=CB(2)
                    END IF
                    NC=(LIMS(KS,2)+K-NEL1-1)*NCX+1
                    Y=0.5*(CG(4,NC)+CG(5,NC))
                END IF
                SS(1)=0.
                DO L=1,MNSP
                    SS(1)=SS(1)+CSS(1,K,L)
                END DO
                WRITE (4,99002) X,Y,SS(1),(CSS(1,K,L)/SS(1),L=1,MNSP)
99002           FORMAT (2F12.5,F12.1,6F12.6)
            END DO

            WRITE (4,*) '   X COORD    Y COORD    NUM FLUX   INC PRESS  REFL PRESS    INCSH STR   '//&
				'REFL SH STR INC TR EN  REFL TR EN  INC ROT EN  REFL ROT EN NET HEAT FLUX'
            DO K=NEL1,NEL2
                IF (ISURF(KS)<3) THEN
                    IF (LIMS(KS,1)<=NCY) THEN
                        NC=(LIMS(KS,1)-1)*NCX+1
                        Y=CG(4,NC)
                    ELSE
                        Y=CB(4)
                    END IF
                    NC=LIMS(KS,2)+K-NEL1
                    X=0.5*(CG(1,NC)+CG(2,NC))
                    AR=2.*PI*CG(3,NC)
                END IF
                IF (ISURF(KS)>2) THEN
                    IF (LIMS(KS,1)<=NCX) THEN
                        IF (ISURF(KS)==3) THEN
                            X=CG(1,LIMS(KS,1))
                        ELSE
                            X=CG(2,LIMS(KS,1))
                        END IF
                    ELSE
                        X=CB(2)
                    END IF
                    NC=(LIMS(KS,2)+K-NEL1-1)*NCX+1
                    Y=0.5*(CG(4,NC)+CG(5,NC))
                    AR=PI*(CG(5,NC)**2-CG(4,NC)**2)
                END IF
                DO N=1,9
                    SS(N)=0.
                    DO L=1,MNSP
                        SS(N)=SS(N)+CSS(N,K,L)
                    END DO
                END DO
                DO N=1,9
                    SS(N)=SS(N)*A/AR
                END DO
                WRITE (4,99003) X,Y,(SS(N),N=1,4),SS(9),SS(5),SS(6),SS(7),SS(8),SS(5)+SS(6)+SS(7)+SS(8)
99003           FORMAT (14E12.5)
            END DO
        END IF
    END DO

    WRITE (4,*) ' FLOWFIELD PROPERTIES '
    WRITE (4,*) 'SAMPLES'
    WRITE (4,*) ' CELL    MEAN SQ. FL    SAMPLE  SP 1     SAMPLE SP 2     ETC '
    DO N=1,MNC
        SS(1)=0.000001
        DO L=1,MNSP
            SS(1)=SS(1)+CS(8,N,L)/REAL(NSMP)
        END DO
        FLM=(CS(9,N,1)/REAL(NSMP)-SS(1)*SS(1))/SS(1)
        WRITE (4,99004) N,FLM,(CS(8,N,L),L=1,MNSP)
    END DO
99004   FORMAT (' ',I6,E12.4,5F9.0)

    WRITE (4,*) ' FLOWFIELD PROPERTIES'
    WRITE (4,*) '  CELL   X COORD   Y COORD    DENSITY    TR TEMP   ROT TEMP   OVTEMP    U         V         W    MACH'
!--first the mixture properties
    RBEGIN1=0
    RBEGIN2=0
    DO N=1,MNC
        A=FNUM/(CC(N)*NSMP)
        SN=0.
        SM=0.
        DO K=1,3
            SMU(K)=0.
        END DO
        SMCC=0.
        SRE=0.
        SRDF=0.
        DO L=1,MNSP
            SN=SN+CS(1,N,L)
!--SN is the number sum
            SM=SM+SP(5,L)*CS(1,N,L)
!--SM is the sum of molecular masses
            DO K=1,3
                SMU(K)=SMU(K)+SP(5,L)*CS(K+1,N,L)
!--SMU(1 to 3) are the sum of mu, mv, mw
            END DO
            SMCC=SMCC+(CS(5,N,L)+CS(6,N,L)+CS(7,N,L))*SP(5,L)
!--SMCC is the sum of m(u**2+v**2+w**2)
            SRE=SRE+CSR(N,L)
!--SRE is the sum of rotational energy
            SRDF=SRDF+ISPR(1,L)*CS(1,N,L)
!--SRDF is the sum of the rotational degrees of freedom
            SUU=SUU+SP(5,L)*CS(5,N,L)
!--SUU is the sum of m*u*u
        END DO
        DENN=SN*A
!--DENN is the number density, see eqn (1.34)
        DEN=DENN*SM/SN
!--DEN is the density, see eqn (1.42)
        DO K=1,3
            VEL(K)=SMU(K)/SM
            SVEL(K,N)=VEL(K)
        END DO
!--VEL and SVEL are the stream velocity components, see eqn (1.43)
        UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
        TT=(SMCC-SM*UU)/(3.D00*DBOLTZ*SN)
!--TT is the translational temperature, see eqn (1.51)
        IF (SRDF>1.E-6) TROT=(2.D00/DBOLTZ)*SRE/SRDF
!--TROT is the rotational temperature, see eqn (11.11)
        TEMP=(3.D00*TT+(SRDF/SN)*TROT)/(3.+SRDF/SN)
        IF (TEMP==0.)  THEN
            MACH=0.
        ELSE
            MACH=SQRT(0.03975*UU/TEMP)
        END IF
!--TEMP is the overall temperature, see eqn (11.12)
        CT(N)=TEMP
        XC=0.5*(CG(1,N)+CG(2,N))
        YC=0.5*(CG(4,N)+CG(5,N))
!--XC,YC are the x,y coordinates of the midpoint of the cell
        WRITE (4,99005) N,XC,YC,DEN,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3),MACH
        WRITE (5,99007) XC,YC,DEN,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3),MACH
99005   FORMAT (' ',I5,2F10.4,1P,E12.4,0P,7F10.4)
99007   FORMAT (' ',2F9.4,1P,2E12.4,0P,9F10.4)
        IF ((N>5280.AND.N<=5760) .AND. RBEGIN1==0) THEN
            WRITE (7,*) YC
            RBEGIN1=1
        END IF
        IF (N>5280.AND.N<=5760) THEN
            WRITE (7,"(6E20.10)") XC,VEL(1),VEL(2),VEL(3),DEN,TEMP
        END IF
        IF ((N>5760.AND.N<=6240) .AND. RBEGIN2==0) THEN
            WRITE (8,*) YC
            RBEGIN2=1
        END IF
        IF (N>5760.AND.N<=6240) THEN
            WRITE (8,"(6E20.10)") XC,VEL(1),VEL(2),VEL(3),DEN,TEMP
        END IF
    END DO

    WRITE (4,*)
    DO L=1,MNSP
!--now the properties of the separate species
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) ' CELL   X COORD    Y COORD   N DENS     DENSITY     TTX       TTY      T     TZ    '//&
			'TR TEMP   ROT TEMP    TEMP   U DIF VEL V DIF VEL W DIF VEL '
        DO N=1,MNC
            A=FNUM/(CC(N)*NSMP)
            DENN=CS(1,N,L)*A
!--DENN is the partial number density
            DEN=SP(5,L)*DENN
!--DEN is the partial density, see eqn (1.13)
            DO K=1,3
                VEL(K)=CS(K+1,N,L)/CS(1,N,L)
!--VEL defines the average velocity of the species L molecules
            END DO
            UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
            TTX=(SP(5,L)/DBOLTZ)*(CS(5,N,L)/CS(1,N,L)-VEL(1)**2)
            TTY=(SP(5,L)/DBOLTZ)*(CS(6,N,L)/CS(1,N,L)-VEL(2)**2)
            TTZ=(SP(5,L)/DBOLTZ)*(CS(7,N,L)/CS(1,N,L)-VEL(3)**2)
!--the component temperatures are based on eqn (1.30)
            TT=(SP(5,L)/(3.D00*DBOLTZ))*((CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)-UU)
!--TT is the translational temperature, see eqn (1.29)
            IF (ISPR(1,L)>0) THEN
                TROT=2.D00*CSR(N,L)/(ISPR(1,L)*DBOLTZ*CS(1,N,L))
            ELSE
                TROT=0.
            END IF
!--TROT is the rotational temperature, see eqn (11.10)
            TEMP=(3.D00*TT+ISPR(1,L)*TROT)/(3.+ISPR(1,L))
            DO K=1,3
                VEL(K)=VEL(K)-SVEL(K,N)
!--VEL now defines the diffusion velocity of species L, see eqn (1.45)
            END DO
            XC=0.5*(CG(1,N)+CG(2,N))
            YC=0.5*(CG(4,N)+CG(5,N))
            WRITE (4,99006) N,XC,YC,DENN,DEN,TTX,TTY,TTZ,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3)
99006       FORMAT (' ',I5,2F9.4,1P,2E12.4,0P,9F10.4)
        END DO
    END DO

    CLOSE (4)
    CLOSE (5)
    CLOSE (7)
    CLOSE (8)

END SUBROUTINE OUTJET
!SROT.FOR

SUBROUTINE SROT(PR,TEMP,IDF)
!--selects a typical equuilibrium value of the rotational energy PR at
!----the temperature TEMP in a gas with IDF rotl. deg. of f.

    USE CONST2
    IMPLICIT NONE
    INTEGER :: IDF
    REAL :: PR,TEMP
    REAL :: A,ERM,B
    REAL,EXTERNAL :: RF
 
    IF (IDF==2) THEN
        PR=-LOG(RF(0))*BOLTZ*TEMP
!--for 2 degrees of freedom, the sampling is directly from eqn (11.22)
    ELSE
!--otherwise apply the acceptance-rejection method to eqn (11.23)
        A=0.5*IDF-1.
        DO
            ERM=RF(0)*10.
!--the cut-off internal energy is 10 kT
            B=((ERM/A)**A)*EXP(A-ERM)
            IF (B<RF(0)) CYCLE
            EXIT
        END DO
        PR=ERM*BOLTZ*TEMP
    END IF
END SUBROUTINE SROT
!ERF.FOR

FUNCTION ERF(S)

!--calculates the error function of S

    REAL :: B,D,C,T,ERF
    B=ABS(S)
    IF (B>4.) THEN
        D=1.
    ELSE
        C=EXP(-B*B)
        T=1./(1.+0.3275911*B)
        D=1.-(0.254829592*T-0.284496736*T*T+1.421413741*T*T*T-1.453152027*T*T*T*T+1.061405429*T*T*T*T*T)*C
    END IF
    IF (S<0.) D=-D
    ERF=D
END FUNCTION ERF
!INDEXM.FOR

SUBROUTINE INDEXM

!--the NM molecule numbers are arranged in order of the molecule groups
!--and, within the groups, in order of the cells and, within the cells,
!--in order of the sub-cells

    USE MOLS2
    USE CELL2
    USE GAS
    IMPLICIT NONE
    INTEGER :: MM,NN,N,LS,MG,MSC,MC,M,L,K

    DO MM=1,MNSG
        IG(2,MM)=0
        DO NN=1,MNC
            IC(2,NN,MM)=0
        END DO
        DO NN=1,MNSC
            ISCG(2,NN,MM)=0
        END DO
    END DO
    DO N=1,NM
        IF (MNSP>1) THEN
            LS=IPS(N)
        ELSE
            LS=1
        END IF
        MG=ISP(LS)
        IG(2,MG)=IG(2,MG)+1
        MSC=IPL(N)
        ISCG(2,MSC,MG)=ISCG(2,MSC,MG)+1
        MC=ISC(MSC)
        IC(2,MC,MG)=IC(2,MC,MG)+1
    END DO
!--number in molecule groups in the cells and sub-cells have been counte
    M=0
    DO L=1,MNSG
        IG(1,L)=M
!--the (start address -1) has been set for the groups
        M=M+IG(2,L)
    END DO
    DO L=1,MNSG
        M=IG(1,L)
        DO N=1,MNC
            IC(1,N,L)=M
            M=M+IC(2,N,L)
        END DO
!--the (start address -1) has been set for the cells
        M=IG(1,L)
        DO N=1,MNSC
            ISCG(1,N,L)=M
            M=M+ISCG(2,N,L)
            ISCG(2,N,L)=0
        END DO
    END DO
!--the (start address -1) has been set for the sub-cells
 
    DO N=1,NM
        IF (MNSP>1) THEN
            LS=IPS(N)
        ELSE
            LS=1
        END IF
        MG=ISP(LS)
        MSC=IPL(N)
        ISCG(2,MSC,MG)=ISCG(2,MSC,MG)+1
        K=ISCG(1,MSC,MG)+ISCG(2,MSC,MG)
        IR(K)=N
!--the molecule number N has been set in the cross-reference array
    END DO
END SUBROUTINE INDEXM
!SELECTJET.FOR

SUBROUTINE SELECTJET
!--selects a potential collision pair and calculates the product of the
!--collision cross-section and relative speed

    USE MOLS2
    USE CELL2
    USE GAS
    USE CONST2
    USE ELAST
    IMPLICIT NONE
    INTEGER :: K,NATT,MSC,NST,NSG,INC
    REAL,EXTERNAL :: RF

    K=INT(RF(0)*(IC(2,N,NN)-0.001))+IC(1,N,NN)+1
    L=IR(K)
!--the first molecule L has been chosen at random from group NN in cell
    NATT=0
    DO
        NATT=NATT+1
        MSC=IPL(L)
        IF ((NN==MM.AND.ISCG(2,MSC,MM)==1).OR.(NN/=MM.AND.ISCG(2,MSC,MM)==0)) THEN
!--if MSC has no type MM molecule find the nearest sub-cell with one
            NST=1
            NSG=1
            DO
                INC=NSG*NST
                NSG=-NSG
                NST=NST+1
                MSC=MSC+INC
                IF (MSC<1.OR.MSC>MNSC) CYCLE
                IF (ISC(MSC)/=N.OR.ISCG(2,MSC,MM)<1) CYCLE
                EXIT
            END DO
        END IF
!--the second molecule M is now chosen at random from the group MM
!--molecules that are in the sub-cell MSC
        K=INT(RF(0)*(ISCG(2,MSC,MM)-0.001))+ISCG(1,MSC,MM)+1
        M=IR(K)
        IF (L==M) CYCLE
!--choose a new second molecule if the first is again chosen

        DO K=1,3
            VRC(K)=PV(K,L)-PV(K,M)
        END DO
!--VRC(1 to 3) are the components of the relative velocity
        VRR=VRC(1)**2+VRC(2)**2+VRC(3)**2
        IF (VRR<1.E-6) THEN
!--attempted collision between identical molecules, this is due to
!----duplication so choose another as long as there is no infinite loop
            IF (NATT<10) CYCLE
            VRR=1.E-6
        END IF
        EXIT
    END DO
    VR=SQRT(VRR)
!--VR is the relative speed
    IF (MNSP>1) THEN
        LS=IPS(L)
        MS=IPS(M)
    ELSE
        LS=1
        MS=1
    END IF
    CVR=VR*SPM(1,LS,MS)*((2.*BOLTZ*SPM(2,LS,MS)/(SPM(5,LS,MS)*VRR))**(SPM(3,LS,MS)-0.5))/SPM(6,LS,MS)
!--the collision cross-section is based on eqn (4.63)
END SUBROUTINE SELECTJET
!ELASTIC.FOR

SUBROUTINE ELASTIC

!--generate the post-collision velocity components.

    USE MOLS2
    USE GAS
    USE CONST2
    USE ELAST
    IMPLICIT NONE

    REAL :: VRCP(3),VCCM(3)
!--VRCP(3) are the post-collision components of the relative velocity
!--VCCM(3) are the components of the centre of mass velocity

    INTEGER :: K
    REAL :: RML,RMM,A,B,C,D,OC,SC
    REAL,EXTERNAL :: RF

    RML=SPM(5,LS,MS)/SP(5,MS)
    RMM=SPM(5,LS,MS)/SP(5,LS)
    DO K=1,3
        VCCM(K)=RML*PV(K,L)+RMM*PV(K,M)
    END DO
!--VCCM defines the components of the centre-of-mass velocity, eqn (2.1)
    IF (ABS(SPM(4,LS,MS)-1.)<1.E-3) THEN
!--use the VHS logic
        B=2.*RF(0)-1.
!--B is the cosine of a random elevation angle
        A=SQRT(1.-B*B)
        VRCP(1)=B*VR
        C=2.*PI*RF(0)
!--C is a random azimuth angle
        VRCP(2)=A*COS(C)*VR
        VRCP(3)=A*SIN(C)*VR
    ELSE
!--use the VSS logic
        B=2.*(RF(0)**SPM(4,LS,MS))-1.
!--B is the cosine of the deflection angle for the VSS model, eqn (11.8)
        A=SQRT(1.-B*B)
        C=2.*PI*RF(0)
        OC=COS(C)
        SC=SIN(C)
        D=SQRT(VRC(2)**2+VRC(3)**2)
        IF (D>1.E-6) THEN
            VRCP(1)=B*VRC(1)+A*SC*D
            VRCP(2)=B*VRC(2)+A*(VR*VRC(3)*OC-VRC(1)*VRC(2)*SC)/D
            VRCP(3)=B*VRC(3)-A*(VR*VRC(2)*OC+VRC(1)*VRC(3)*SC)/D
        ELSE
            VRCP(1)=B*VRC(1)
            VRCP(2)=A*OC*VRC(1)
            VRCP(3)=A*SC*VRC(1)
        END IF
!--the post-collision rel. velocity components are based on eqn (2.22)
    END IF
!--VRCP(1 to 3) are the components of the post-collision relative vel.
    DO K=1,3
        PV(K,L)=VCCM(K)+VRCP(K)*RMM
        PV(K,M)=VCCM(K)-VRCP(K)*RML
    END DO
END SUBROUTINE ELASTIC
!RVELC.FOR

SUBROUTINE RVELC(U,V,VMP)

!--generates two random velocity components U an V in an equilibrium
!--gas with most probable speed VMP  (based on eqns (C10) and (C12))

    IMPLICIT NONE
    REAL :: U,V,VMP
    REAL :: A,B
    REAL,EXTERNAL :: RF

    A=SQRT(-LOG(RF(0)))
    B=6.283185308*RF(0)
    U=A*SIN(B)*VMP
    V=A*COS(B)*VMP
END SUBROUTINE RVELC
!GAM.FOR

FUNCTION GAM(X)

!--calculates the Gamma function of X.

    REAL :: A,Y,GAM
    A=1.
    Y=X
    IF (Y<1.) THEN
        A=A/Y
    ELSE
        DO
            Y=Y-1
            IF (Y>=1.) THEN
                A=A*Y
                CYCLE
            END IF
            EXIT
        END DO
    END IF
    GAM=A*(1.-0.5748646*Y+0.9512363*Y**2-0.6998588*Y**3+0.4245549*Y**4-0.1010678*Y**5)
END FUNCTION GAM
!COLLMR.FOR

SUBROUTINE COLLMR

!--calculates collisions appropriate to DTM in a gas mixture

    USE MOLS2
    USE MOLSR
    USE CELL2
    USE GAS
    USE GASR
    USE GEOMJET
    USE SAMPJET
    USE SAMPR
    USE COMP
    USE CONST2
    USE ELAST
    IMPLICIT NONE

    INTEGER :: K,NSEL,ISEL
    REAL :: SN,AVN,ASEL,CVM
    REAL,EXTERNAL :: RF

!--VRC(3) are the pre-collision components of the relative velocity

    DO N=1,MNC
!--consider collisions in cell N
        DO NN=1,MNSG
            DO MM=1,MNSG
                SN=0.
                DO K=1,MNSP
                    IF (ISP(K)==MM) SN=SN+CS(1,N,K)
                END DO
                IF (SN>1.) THEN
                    AVN=SN/REAL(NSMP)
                ELSE
                    AVN=IC(2,N,MM)
                END IF
!--AVN is the average number of group MM molecules in the cell
                ASEL=0.5*IC(2,N,NN)*AVN*FNUM*CCG(1,N,NN,MM)*DTM/CC(N)+CCG(2,N,NN,MM)
!--ASEL is the number of pairs to be selected, see eqn (11.5)
                NSEL=ASEL
                CCG(2,N,NN,MM)=ASEL-NSEL
                IF (NSEL>0) THEN
                    IF (((NN/=MM).AND.(IC(2,N,NN)<1.OR.IC(2,N,MM)<1)).OR.((NN==MM).AND.(IC(2,N,NN)<2))) THEN
                        CCG(2,N,NN,MM)=CCG(2,N,NN,MM)+NSEL
!--if there are insufficient molecules to calculate collisions,
!--the number NSEL is added to the remainer CCG(2,N,NN,MM)
                    ELSE
                        CVM=CCG(1,N,NN,MM)
                        SELT=SELT+NSEL
                        DO ISEL=1,NSEL

                            CALL SELECTJET

                            IF (CVR>CVM) CVM=CVR
!--if necessary, the maximum product in CVM is upgraded
                            IF (RF(0)<CVR/CCG(1,N,NN,MM)) THEN
!--the collision is accepted with the probability of eqn (11.6)
                                NCOL=NCOL+1
                                SEPT=SEPT+SQRT((PP(1,L)-PP(1,M))**2+(PP(2,L)-PP(2,M))**2)
                                COL(LS,MS)=COL(LS,MS)+1.D00
                                COL(MS,LS)=COL(MS,LS)+1.D00

                                IF (ISPR(1,LS)>0.OR.ISPR(1,MS)>0) CALL INELR
!--bypass rotational redistribution if both molecules are monatomic

                                CALL ELASTIC

                            END IF
                        END DO
                        CCG(1,N,NN,MM)=CVM
                    END IF
                END IF
            END DO
        END DO
    END DO
END SUBROUTINE COLLMR
!INELR.FOR

SUBROUTINE INELR

!--adjustment of rotational energy in a collision

    USE MOLSR
    USE GAS
    USE GASR
    USE ELAST
    IMPLICIT NONE

    INTEGER :: NSP,K,KS,JS,IRT,IR(2)
    REAL :: ETI,ECI,ECF,ECC,XIB,ATK,ERM,XIA,ETF,A
    REAL,EXTERNAL :: RF

!--IR is the indicator for the rotational redistribution
    ETI=0.5*SPM(5,LS,MS)*VRR
!--ETI is the initial translational energy
    ECI=0.
!--ECI is the initial energy in the active rotational modes
    ECF=0.
!--ECF is the final energy in these modes
    ECC=ETI
!--ECC is the energy to be divided
    XIB=2.5-SPM(3,LS,MS)
!--XIB is th number of modes in the redistribution
    IRT=0
!--IRT is 0,1 if no,any redistribution is made
    DO NSP=1,2
!--consider the molecules in turn
        IF (NSP==1) THEN
            K=L
            KS=LS
            JS=MS
        ELSE
            K=M
            KS=MS
            JS=LS
        END IF
        IR(NSP)=0
        IF (ISPR(1,KS)>0) THEN
            IF (ISPR(2,KS)==0) THEN
                ATK=1./SPR(1,KS,JS)
            ELSE
                ATK=1./(SPR(1,KS,JS)+SPR(2,KS,JS)*CT(N)+SPR(3,KS,JS)*CT(N)**2)
            END IF
!--ATK is the probability that rotation is redistributed to molecule L
            IF (ATK>RF(0)) THEN
                IRT=1
                IR(NSP)=1
                IF (MNMR>1) THEN
                    ECC=ECC+PR(K)
                    ECI=ECI+PR(K)
                END IF
                XIB=XIB+0.5*ISPR(1,KS)
            END IF
        END IF
    END DO
!--apply the general Larsen-Borgnakke distribution function
    IF (IRT==1) THEN
        DO NSP=1,2
            IF (IR(NSP)==1) THEN
                IF (NSP==1) THEN
                    K=L
                    KS=LS
                ELSE
                    K=M
                    KS=MS
                END IF
                XIB=XIB-0.5*ISPR(1,KS)
!--the current molecule is removed from the total modes
                IF (ISPR(1,KS)==2) THEN
                    ERM=1.-RF(0)**(1./XIB)
                ELSE
                    XIA=0.5*ISPR(1,KS)
                    CALL LBS(XIA-1.,XIB-1.,ERM)
                END IF
                IF (MNMR>1) THEN
                    PR(K)=ERM*ECC
                    ECC=ECC-PR(K)
!--the available energy is reduced accordingly
                    ECF=ECF+PR(K)
                END IF
            END IF
        END DO
        ETF=ETI+ECI-ECF
!--ETF  is the post-collision translational energy
!--adjust VR and, for the VSS model, VRC for the change in energy
        A=SQRT(2.*ETF/SPM(5,LS,MS))
        IF (ABS(SPM(4,LS,MS)-1.)<1.E-3) THEN
            VR=A
        ELSE
            DO K=1,3
                VRC(K)=VRC(K)*A/VR
            END DO
            VR=A
        END IF
    END IF
END SUBROUTINE INELR
!LBS.FOR

SUBROUTINE LBS(XMA,XMB,ERM)
!--selects a Larsen-Borgnakke energy ratio using eqn (11.9)
    IMPLICIT NONE
    REAL :: XMA,XMB,ERM
    REAL :: P
    REAL,EXTERNAL :: RF

    DO
        ERM=RF(0)
        IF (XMA<1.E-6.OR.XMB<1.E-6) THEN
            IF (XMA<1.E-6.AND.XMB<1.E-6) EXIT
            IF (XMA<1.E-6) P=(1.-ERM)**XMB
            IF (XMB<1.E-6) P=(1.-ERM)**XMA
        ELSE
            P=(((XMA+XMB)*ERM/XMA)**XMA)*(((XMA+XMB)*(1.-ERM)/XMB)**XMB)
        END IF
        IF (P<RF(0)) CYCLE
        EXIT
    END DO
END SUBROUTINE LBS
!AIFR.FOR

SUBROUTINE AIFR(YI,DY,DZ,Y,V,W)
!--calculates the new radius and realigns the velocity components in
!----the axially symmetric flow
    IMPLICIT NONE
    REAL :: YI,DY,DZ,Y,V,W
    REAL :: DR,VR,A,S,C,B

    DR=DZ
    VR=W
    A=YI+DY
    Y=SQRT(A*A+DR*DR)
    S=DR/Y
    C=A/Y
    B=V
    V=B*C+VR*S
    W=-B*S+VR*C
END SUBROUTINE AIFR
!RF.FOR

FUNCTION RF(IDUM)
!--generates a uniformly distributed random fraction between 0 and 1
!----IDUM will generally be 0, but negative values may be used to
!------re-initialize the seed
    INTEGER,SAVE :: MA(55),INEXT,INEXTP
    INTEGER,PARAMETER :: MBIG=1000000000,MSEED=161803398,MZ=0
    REAL,PARAMETER :: FAC=1.E-9
    DATA IFF/0/
    INTEGER :: MJ,MK,I,II,K
    REAL :: RF

    IF (IDUM<0.OR.IFF==0) THEN
        IFF=1
        MJ=MSEED-IABS(IDUM)
        MJ=MOD(MJ,MBIG)
        MA(55)=MJ
        MK=1
        DO I=1,54
            II=MOD(21*I,55)
            MA(II)=MK
            MK=MJ-MK
            IF (MK<MZ) MK=MK+MBIG
            MJ=MA(II)
        END DO
        DO K=1,4
            DO I=1,55
                MA(I)=MA(I)-MA(1+MOD(I+30,55))
                IF (MA(I)<MZ) MA(I)=MA(I)+MBIG
            END DO
        END DO
        INEXT=0
        INEXTP=31
    END IF
    DO
        INEXT=INEXT+1
        IF (INEXT==56) INEXT=1
        INEXTP=INEXTP+1
        IF (INEXTP==56) INEXTP=1
        MJ=MA(INEXT)-MA(INEXTP)
        IF (MJ<MZ) MJ=MJ+MBIG
        MA(INEXT)=MJ
        RF=MJ*FAC
        IF (RF>1.E-8.AND.RF<0.99999999) EXIT
    END DO
END FUNCTION RF
!DATAJET.FOR

SUBROUTINE DATAJET

!--defines the data for a particular run of DSMCJET.FOR.

    USE GAS
    USE GASR
    USE CELL2
    USE SAMPJET
    USE COMP
    USE GEOMJET
    IMPLICIT NONE

!--set data (must be consistent with PARAMETER variables)

    OPEN (2,FILE='INPUT.DAT',STATUS='OLD',FORM='FORMATTED')
    READ (2,*)
    READ (2,*) IWF,RWF,NCX,NCY,NSCX,NSCY,IFCX,IFCY,CWRY,IIS,ISG,LFLX,LFLY
    READ (2,*)
    READ (2,*) FTMP,FND,FFND,VFW,FSP(1),FNUM,DTM,CB(1),CB(2),CB(3),CB(4)
    READ (2,*)
    READ (2,*) IB(1),IB(2),IB(3),IB(4),ISURF(1),LIMS(1,1),LIMS(1,2),LIMS(1,3),TSURF(1),WSURF(1),ALPI(1)
    READ (2,*)
    READ (2,*) ISURF(2),LIMS(2,1),LIMS(2,2),LIMS(2,3),TSURF(2),WSURF(2),ALPI(2),&
		ISURF(3),LIMS(3,1),LIMS(3,2),LIMS(3,3),TSURF(3),WSURF(3),ALPI(3)
    READ (2,*)
    READ (2,*) ISURF(4),LIMS(4,1),LIMS(4,2),LIMS(4,3),TSURF(4),WSURF(4),ALPI(4),&
		IJET,LIMJ(1),LIMJ(2),LIMJ(3),TMPJ,FNDJ,FUJ,FVJ,FWJ,FSPJ(1)
    READ (2,*)
    READ (2,*) SP(1,1),SP(2,1),SP(3,1),SP(4,1),SP(5,1),ISPR(1,1),SPR(1,1,1),ISPR(2,1),ISP(1),NIS,NSP,NPT
    CLOSE (2)
END SUBROUTINE DATAJET