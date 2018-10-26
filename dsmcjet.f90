module const1
    implicit none
    integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
contains
    subroutine const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        open (1,file='parameter.dat',status='old',form='formatted')
        read (1,*)
        read (1,*) mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        close (1)
    end subroutine const
end module const1

module mols2
    use const1
    implicit none
    integer :: nm
    integer,allocatable :: ipl(:),ips(:),ir(:)
    real,allocatable :: pp(:,:),pv(:,:)
contains
    subroutine all1
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(ipl(mnm),ips(mnms),ir(mnm),pp(2,mnm),pv(3,mnm))
    end subroutine all1
end module mols2

module molsr
    use const1
    implicit none
    real,allocatable :: pr(:)
contains
    subroutine all2
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(pr(mnmr))
    end subroutine all2
end module molsr

module mold
    use const1	
    implicit none
    integer :: nmb
    integer,allocatable :: iplb(:),ipsb(:),irb(:)
    real,allocatable :: ppb(:,:),pvb(:,:),prb(:)
contains
    subroutine all3
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(iplb(mnb),ipsb(mnb),irb(mnb),ppb(2,mnb),pvb(3,mnb),prb(mnb))
    end subroutine all3
end module mold

module cell2
    use const1
    implicit none
    integer :: ncx,ncy,ifcx,ifcy
    real :: cwrx,cwry,apx,rpx,apy,rpy
    integer,allocatable :: ic(:,:,:),isc(:),iscg(:,:,:),ig(:,:)
    real,allocatable :: cc(:),cg(:,:),ccg(:,:,:,:)
contains
    subroutine all4
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(ic(2,mnc,mnsg),isc(mnsc),iscg(2,mnsc,mnsg),ig(2,mnsg),cc(mnc),cg(6,mnc),ccg(2,mnc,mnsg,mnsg))
    end subroutine all4
end module cell2

module gas
    use const1
    implicit none
    integer,allocatable :: isp(:)
    real,allocatable :: sp(:,:),spm(:,:,:)
contains
    subroutine all5
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(isp(mnsp),sp(5,mnsp),spm(6,mnsp,mnsp))
    end subroutine all5
end module gas

module gasr
    use const1
    implicit none
    integer,allocatable :: ispr(:,:)
    real,allocatable :: spr(:,:,:),ct(:)
contains
    subroutine all6
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(ispr(3,mnsp),spr(3,mnsp,mnsp),ct(mnc))
    end subroutine all6
end module gasr

module sampjet
    use const1
    implicit none
    integer :: npr,nsmp,ispd
    real*8 :: movt,ncol,selt,sept
    real :: time,fnd,ffnd,ftmp,timi,vfw
    real*8,allocatable :: col(:,:),cs(:,:,:)
    real,allocatable :: fsp(:)
contains
    subroutine all7
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(col(mnsp,mnsp),cs(9,mnc,mnsp),fsp(mnsp))
    end subroutine all7
end module sampjet

module sampr
    use const1
    implicit none
    real*8,allocatable :: csr(:,:)
contains
    subroutine all8
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(csr(mnc,mnsp))
    end subroutine all8
end module sampr

module samps
    use const1
    implicit none
    real*8,allocatable :: css(:,:,:)
contains
    subroutine all9
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(css(9,mnse,mnsp))
    end subroutine all9
end module samps

module comp
    implicit none
    integer :: nis,nsp,npt
    real :: fnum,dtm
end module comp

module geomjet
    use const1
    implicit none
    integer :: nscx,nscy,ib(4),isurf(4),lims(4,3),iis,isg,ijet,limj(3),lflx,lfly,iwf
    real :: cb(4),tsurf(4),tmpj,fndj,fuj,fvj,fwj,cw,fw,ch,fh,wj,alpi(4),alpn(4),alpt(4),rwf,wsurf(4)
    real,allocatable :: fspj(:),amej(:,:),amrj(:,:),ame(:,:,:),amr(:,:,:),vefu(:),vefv(:),vefw(:),rou_bound(:),t_bound(:)
contains
    subroutine all10
        implicit none
        integer :: mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls
        call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
        allocate(fspj(mnsp),amej(mnsp,mbc),amrj(mnsp,mbc),ame(4,mnsp,mbc),amr(4,mnsp,mbc),vefu(mbc),vefv(mbc),vefw(mbc),rou_bound(mbc),t_bound(mbc))
    end subroutine all10
end module geomjet

module const2
    implicit none
    real :: pi,spi,boltz
end module const2

module elast
    implicit none
    integer :: l,m,ls,ms,mm,nn,n
    real :: vrc(3),vrr,vr,cvr
end module elast

!dsmcjet.for

program dsmcjet

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
!------or follow the cercignani-lampis-lord (cll) model
!------the surface may be moving in the circumferential direction
!------(the cll model cannot be used with circumferential velocities)
!----there may be one uniform jet
!------this must lie along a cell boundary

!--si units are used throughout

!-------------------------description of data---------------------------

!--parameter variables must be consistent with data in subroutine datajet

!--iwf 0,1 for no weighting factors, or factors proportional to radius
!--rwf the reference radius below which the weighting factors are unity
!----required only for iwf=1, very small values are not recommended

!--ncx the number of cells in the x direction (cell columns)
!--ncy the number of cells in the y direction (cell rows)
!----(mnc must be equal to or greater than ncx*ncy)

!--nscx the number of sub-cells per cell in the x direction
!--nscy the number of sub-cells per cell in the y direction
!----(mnsc must be at least mnc*nscx*nscy)

!--ifcx set to 0 or 1 for uniform or non-uniform cell widths in x dirn.
!---if ifcx=1, set cwrx as the ratio of the cell width at the outer x
!----boundary to that at the inner x boundary (default 0)
!--ifcy and cwry similar to ifcx and cwrx for the y direction

!--iis 0 if there is no stream, 1 if there is a uniform stream
!--isg 0 if there is a stream and the initial state is a vacuum, or
!----1 if the initial state is a uniform stream, or

!--if part of the rectangular region is excluded from the flow, set
!--lflx if positive no flow below where lfly condition is also satisfied
!------ if negative no flow above where lfly condition is also satisfied
!--lfly if positive no flow below where lflx condition is also satisfied
!------ if negative no flow above where lflx condition is also satisfied
!--for example, if lflx=3 and lfly=-2, there is no stream in the corner
!----below cell column 3 and above cell row 2
!--if no flow is excluded, lflx,y are automatically set to 0

!--ftmp the stream temperature if iis=1, or a temperature characteristic
!----of the flow otherwise (if ftmp is not set for iis= 0, the
!----default value of 273 is used to set the initial value of ccg(1

!--fnd the initial number density for iis=1
!----or need not be set for iis=0

!--vfx the stream velocity in the x direction
!--need not be set for iis=0

!--fsp(l) the fraction (by number) of species l in the initial stream
!----a value is requred for each species, but need not be set for iis=0

!--fnum the number of real mols. represented by each simulated molecule

!--dtm the time step over which the motion and collisions are uncoupled

!--the following data is required for each boundary
!----k=1 for the lower value of x
!----k=2 for the higher value of x
!----k=3 for the lower value of y
!--if a boundary is on the axis, it must be boundary 3
!----k=4 for the higher value of y

!--cb(k) the coordinate of the boundary (x for k=1 or 2, y for k=3 or 4)

!--ib(k) the type code of the boundary
!----1 for stream, 2 for plane of symmetry, 3 for vacuum, 4 for axis

!--isurf(k) with k=1, 2 for the two surfaces
!----0 if there is no surface
!----1 if the surface normal is in the positive y direction
!----2 if the surface normal is in the negative y direction
!----3 if the surface normal is in the positive x direction
!----4 if the surface normal is in the negative x direction

!--the following surface data is required if isurf(k) > 0

!--lims(k,l) with k as before, and the surface lies along the
!----lower boundary of the cell row or column given by l=1
!----one edge of the surface is at the lower edge of the row or column
!----given by l=2, and the other is at the upper edge of the row or
!----column given by l=3

!--wsurf(k) the circumferential velocity of the surface
!----applies only to surfaces parallel to the axis and
!----these surfaces must be diffusely reflecting
!--tsurf(k) the temperature of the surface
!----if tsurf is negative, the reflection is specular
!----if tsurf is positive, the following must also be specified:
!--alpi(k) should be -1. for diffuse reflection, or between 0 and 1
!----for the rotational energy accommodation coefficient for cll model
!--alpn(k) the normal momentum accommodation coeff. in the cll model
!--alpt(k) the tangential momentum accommodation coeff. in the cll model
!----(if alpi is not set explicitly, it is set to default value of -1.)

!--end of surface data

!--ijet  0 if there is no jet, or
!----1 if the jet is in the positive y direction
!----2 if the jet is in the negative y direction
!----3 if the jet is in the positive x direction
!----4 if the jet is in the negative x direction

!--if ijet > 0, the following data is required

!--limj(l) the jet efflux plane lies along the
!----lower boundary of the cell row or column given by l=1
!----one edge of the plane is at the lower edge of the row or column
!----given by l=2, and the other is at the upper edge of the row or
!----column given by l=3

!--tmpj the jet temperature

!--fndj the number density of the jet

!--fvj the jet velocity

!--fspj(l) the fraction (by number) of species l in the jet
!----a value is requred for each species

!--end of jet data

!--ispd (required only for gas mixtures) set to 0 if the diameter,
!----viscosity exponent, and vss scattering parameter for the
!----cross-collisions are to be set to the mean values, or
!----set to 1 if these quantities are to be set as data

!--the following data must be repeated for each species (l=1 to mnsp)

!--sp(1,l) the reference diameter
!--sp(2,l) the reference temperature
!--sp(3,l) the viscosity temperature power law
!--sp(4,l) the reciprocal of the vss scattering parameter (1. for vhs)
!--sp(5,l) the molecular mass

!--isp(l) the collision sampling group in which the species lies
!----this must be le.mnsc (not required if mnsg=1)

!--ispr(1,l) the number of rotational degrees of freedom
!--ispr(2,l) 0, 1 for constant, polynomial rotational relaxation number
!--ispr(3,l) 0, 1 for common or collision partner species dependent
!----rotational relaxation rate

!--spr(1,l,k) the constant value, or constant in the polynomial for zr
!----in a collision of species l with species k
!--the following two items are required only if ispr(2,l)=1
!--spr(2,l,k) the coefficient of temperature in the polynomial
!--spr(3,l,k) the coefficient of temperature squared in the polynomial

!--end of data for the individual species

!--the following data on the cross-collisions is required only if ispd=1
!--then only for l.ne.m, but l,m data must be repeated for m,l

!--spm(1,l,m) the reference diameter for species l-m collisions
!--spm(2,l,m) the reference temperature for species l-m collisions
!--spm(3,l,m) the viscosity temperature power law for species l-m colls.
!--spm(4,l,m) the reciprocal of the vss scattering parameter

!--end of species data

!--nis the number of dtm time steps between samplings

!--nsp the number of samples between prints

!--nps the number of prints to the assumed start of steady flow

!--npt the number of prints to stop

!-----------------------------------------------------------------------
	
    use const1
    use mols2
    use molsr
    use mold
    use cell2
    use gas
    use gasr
    use sampjet
    use sampr
    use samps
    use comp
    use geomjet
    use const2
    implicit none
!--mnmr 1 if all molecules are monatomic, mnm otherwise
!--mnms 1 if there is only one species, mnm for a gas mixture
!--mbc the maximum number of cell divisions along any entry boundary
!--mnb the maximum number of molecules in the duplication delay buffer
!--other variables as defined in dsmc2.for

!--cs(8 the sample (unweighted) number
!--cs(9,1 the square of the sample (all species)
!--other variables as defined in dsmc0.for, but all values are weighted

!--csr(m,l) the sum of the rotational energy of species l in cell m

    !integer :: nql,nqls,jjj,iii
    integer :: jjj,iii

!--css(n,m,l) sampled info. on the molecules striking the boundaries
!----m is the code number of the element; l is the species
!----n=1 the weighted number sum
!----n=2 the sum of the normal momentum of the incident molecules
!----n=3 the sum of the normal momentum for the reflected molecules
!----n=4 the sum of the incident tangential momentum
!----n=5 the sum of the incident translational energy
!----n=6 the sum of the reflected translational energy
!----n=7 the sum of the incident rotational energy
!----n=8 the sum of the reflected rotational energy
!----n=9 the sum of the reflected tangential momentum

!--variables as defined in dsmc0.for except that pp( is two-dimensional

!--pr(m) is the rotational energy of molecule m

!--nmb the number of molecules in the duplication delay buffer

!--ifcx,ifcy 0,1 for uniform, geometric progression for cell width
!--cwrx, cwry the ratio of cell width, height at higher valued boundary
!----to that at the lower valued boundary
!--other variables as defined in dsmc0.for except that
!----cg(4,5,6 for y correspond to cg(1,2,3 for x

!--variables as defined in dsmc0.for

!--variables as defined in dsmc0r.for

!--vfx stream velocity component in x direction
!--other variables as defined in dsmc0.for

!--variables as defined in dsmc0.for

!--iis 0, 1 if the initial flow is a vacuum, uniform stream
!--nscx,nscy the number of sub-cells per cell in x,y directions
!--cb(n) the location of the boundary
!----n=1,2 for lower, higher value of x
!----n=3,4 for lower, higher value of y
!--ib(n) n=1 to 4 as above,  the type code for the boundary
!--isurf(k) 0 for no surface, otherwise direction of normal to surface k
!--lims(k,3) defines the location of the surface
!--tsurf(k) the temperature of the surface (negative for specular refl.)
!--wsurf(k) the circumferential velocity of the surface
!--ijet 0 for no jet, otherwise the jet direction
!--limj(k) defines the location of the jet
!--tmpj the jet temperature
!--fndj the number density of the jet
!--fvj the jet velocity
!--fspj(l) the fraction of species l in the jet
!--ame(n,l,k) number of molecules of species l that enter element k of
!----side n each dtm
!--amr(n,l,k) the remainder associated with ame
!--amej(l,k) number of molecules of species l that enter el. k of jet
!--amrj(l,k) the remainder associated with amej
!--cw the cell width for uniform cells
!--fw the flow width
!--alpi(k) the accommodation coefficient for rotational energy for cll
!----model this should be negative if the reflection is to be diffuse
!--alpn(k) the cll accommodation coefficient for normal momentum
!--alpt(k) the cll accommodation coefficient for tangential momentum
!--iwf 0,1 for no weighting factors, factors proportional to radius
!--rwf the reference radius for weighting factors
!----if iwf=1, a simulated molecule at radius r represents r*fnum/rwf
!------real molecules

!--variables as defined in dsmc0.for

    call const(mnm,mnmr,mnms,mnc,mnsc,mnsp,mnsg,mnse,mbc,mnb,nql,nqls)
    call all1
    call all2
    call all3
    call all4
    call all5
    call all6
    call all7
    call all8
    call all9
    call all10

    !write (*,*) ' input 0,1 for continuing,new calculation:- '
    !read (*,*) nql
    !write (*,*) ' input 0,1 for continuing,new sample:- '
    !read (*,*) nqls

    if (nql==1) then

        call initjet

    else

        write (*,*) ' read the restart file'
        open (4,file='dsmcjet.res',status='old',form='unformatted')
        read (4) alpi,alpn,alpt,apx,apy,fnd,ffnd,ame,amej,amr,amrj,boltz,cb,cc,ccg,cg,ch,col,cs,csr,css,ct,cw,cwrx,cwry,&
                 dtm,fndj,fnum,fspj,ftmp,fuj,fvj,fwj,fh,fw,ib,ic,ifcx,ifcy,iis,ijet,ipl,iplb,ips,ipsb,ir,irb,isc,&
                 iscg,isg,isp,ispr,isurf,iwf,lflx,lfly,limj,lims,movt,ncol,ncx,ncy,nis,nm,nmb,nscx,nscy,nsmp,npr,&
                 npt,nsp,pi,pp,ppb,pr,prb,pv,pvb,rpx,rpy,rwf,rou_bound,selt,sept,sp,spi,spm,spr,time,timi,tmpj,t_bound,tsurf,vefu,vefv,vefw,vfw,wj,wsurf
        close (4)
        if (npr==npt) npr=0

    end if

    if (nqls==1) call sampijet

    do
        npr=npr+1

        do jjj=1,nsp
            do iii=1,nis
                time=time+dtm

                write (*,99001) iii,jjj,nis,nsp,nm,ncol
99001           format (' dsmcjet:- move',2i5,' of',2i5,i8,' mols',f14.0,' colls')

                call movejet

                call indexm

                call collmr

            end do

            call samplejet

        end do

        write (*,*) ' writing restart and output files',npr,'  of ',npt
        open (4,file='dsmcjet.res',form='unformatted')
        write (4) alpi,alpn,alpt,apx,apy,fnd,ffnd,ame,amej,amr,amrj,boltz,cb,cc,ccg,cg,ch,col,cs,csr,css,ct,cw,cwrx,cwry,&
                 dtm,fndj,fnum,fspj,ftmp,fuj,fvj,fwj,fh,fw,ib,ic,ifcx,ifcy,iis,ijet,ipl,iplb,ips,ipsb,ir,irb,isc,&
                 iscg,isg,isp,ispr,isurf,iwf,lflx,lfly,limj,lims,movt,ncol,ncx,ncy,nis,nm,nmb,nscx,nscy,nsmp,npr,&
                 npt,nsp,pi,pp,ppb,pr,prb,pv,pvb,rpx,rpy,rwf,rou_bound,selt,sept,sp,spi,spm,spr,time,timi,tmpj,t_bound,tsurf,vefu,vefv,vefw,vfw,wj,wsurf
        close (4)

        call outjet

        if (npr>=npt) exit
    end do
    deallocate(ipl,ips,ir,pp,pv)
    deallocate(pr)
    deallocate(iplb,ipsb,irb,ppb,pvb,prb)
    deallocate(ic,isc,iscg,ig,cc,cg,ccg)
    deallocate(isp,sp,spm)
    deallocate(ispr,spr,ct)
    deallocate(col,cs,fsp)
    deallocate(csr)
    deallocate(css)
    deallocate(fspj,amej,amrj,ame,amr,vefu,vefv,vefw,rou_bound,t_bound)
    
end program dsmcjet
!initjet.for

subroutine initjet

    use mols2
    use molsr
    use mold
    use cell2
    use gas
    use gasr
    use sampjet
    use sampr
    use comp
    use geomjet
    use const2
    implicit none

    integer :: k,j,l,m,n,mx,my,iprob,nx,ny,mm,ncolm,nrow,ncs,nc,ncl,mc
    real :: rem,vmp,wmin,ymid,a,wf,sc,tempvar
    real,allocatable :: tempz(:),tempu(:),tempv(:),tempw(:),temprou(:),tempt(:)
    real,external :: erf,gam,rf
    
    allocate(tempz(231),tempu(231),tempv(231),tempw(231),temprou(231),tempt(231))

!--set constants

    pi=3.141592654
    spi=sqrt(pi)
    boltz=1.380622e-23

!--set data variables to default values that they retain if the data
!----does not reset them to specific values
    iwf=0
    fnd=0.
    ffnd=0.
    ftmp=273.
    vfw=0.
    ifcx=0
    ifcy=0
    lflx=0
    lfly=0
    alpi(1)=-1.
    alpi(2)=-1.
    alpi(3)=-1.
    alpi(4)=-1.
    wsurf(1)=0.
    wsurf(2)=0.
    wsurf(3)=0.
    wsurf(4)=0.
    nmb=0
    do n=1,4
        ib(n)=3
        do l=1,mnsp
            isp(l)=1
            fsp(l)=0.
            do k=1,mbc
                vefu(k)=0.
                vefv(k)=0.
                vefw(k)=0.
                rou_bound(k)=0.
                t_bound(k)=0.
                ame(n,l,k)=0.
                amr(n,l,k)=rf(0)
            end do
        end do
    end do
    do l=1,mnsp
        do k=1,mbc
            amej(l,k)=0.
            amrj(l,k)=rf(0)
        end do
    end do

    call datajet

!--set additional data on the gas

    if (mnsp==1) ispd=0
    do n=1,mnsp
        do m=1,mnsp
            if ((ispr(3,n)==0).and.(m/=n)) then
                spr(1,n,m)=spr(1,n,n)
                spr(2,n,m)=spr(2,n,n)
                spr(3,n,m)=spr(3,n,n)
            end if
            if ((ispd==0).or.(n==m)) then
                spm(1,n,m)=0.25*pi*(sp(1,n)+sp(1,m))**2
!--the collision cross section is assumed to be given by eqn (1.35)
                spm(2,n,m)=0.5*(sp(2,n)+sp(2,m))
                spm(3,n,m)=0.5*(sp(3,n)+sp(3,m))
                spm(4,n,m)=0.5*(sp(4,n)+sp(4,m))
!--mean values are used for ispd=0
            else
                spm(1,n,m)=pi*spm(1,n,m)**2
!--the cross-collision diameter is converted to the cross-section
            end if
            spm(5,n,m)=(sp(5,n)/(sp(5,n)+sp(5,m)))*sp(5,m)
!--the reduced mass is defined in eqn (2.7)
            spm(6,n,m)=gam(2.5-spm(3,n,m))
        end do
    end do

!--initialise variables

    time=0.
    nm=0
    npr=0
    ncol=0
    movt=0.
    selt=0.
    sept=0.

    do m=1,mnsp
        do n=1,mnsp
            col(m,n)=0.
        end do
    end do

    fw=cb(2)-cb(1)
    fh=cb(4)-cb(3)
    cg(1,1)=cb(1)
    if (ifcx==0) then
        cw=fw/ncx
    else
        rpx=cwrx**(1./(ncx-1.))
!--rpx is the ratio in the geometric progression
        apx=(1.-rpx)/(1.-rpx**ncx)
!--ap is the first term of the progression
    end if
    cg(4,1)=cb(3)
    if (ifcy==0) then
        ch=fh/ncy
!--ch is the uniform cell height
    else
        rpy=cwry**(1./(ncy-1.))
!--rpy is the ratio in the geometric progression
        apy=(1.-rpy)/(1.-rpy**ncy)
!--apy is the first term of the progression
    end if
    do my=1,ncy
        do mx=1,ncx
            m=(my-1)*ncx+mx
!--m is the cell number
            ct(m)=ftmp
!--the macroscopic temperature is set to the freestream temperature
!--set the x coordinates
            if (mx==1) cg(1,m)=cg(1,1)
            if (mx>1) cg(1,m)=cg(2,m-1)
            if (ifcx==0) then
                cg(2,m)=cg(1,m)+cw
            else
                cg(2,m)=cg(1,m)+fw*apx*rpx**(mx-1)
            end if
            cg(3,m)=cg(2,m)-cg(1,m)
!--set the y coordinates
            if (my==1) cg(4,m)=cg(4,1)
            if (my>1.and.mx==1) cg(4,m)=cg(5,m-1)
            if (my>1.and.mx>1) cg(4,m)=cg(4,m-1)
            if (ifcy==0) then
                cg(5,m)=cg(4,m)+ch
            else
                cg(5,m)=cg(4,m)+fh*apy*rpy**(my-1)
            end if
            cg(6,m)=cg(5,m)-cg(4,m)
            cc(m)=pi*cg(3,m)*(cg(5,m)**2-cg(4,m)**2)
            do l=1,mnsg
                do k=1,mnsg
                    ccg(2,m,l,k)=rf(0)
                    ccg(1,m,l,k)=spm(1,1,1)*300.*sqrt(ftmp/300.)
                end do
            end do
!--the maximum value of the (rel. speed)*(cross-section) is set to a
!--reasonable, but low, initial value and will be increased as necessary
        end do
    end do

    if (ifcx==1) then
        apx=(1.-rpx)/apx
        rpx=log(rpx)
!--apx and rpx are now the convenient terms in eqn (12.1)
    end if
    if (ifcy==1) then
        apy=(1.-rpy)/apy
        rpy=log(rpy)
!--apy and rpy are now the convenient terms in eqn (12.1)
    end if

!--set sub-cells

    do n=1,mnc
        do m=1,nscy
            do k=1,nscx
                l=(n-1)*nscx*nscy+(m-1)*nscx+k
                isc(l)=n
            end do
        end do
    end do

    if (iis>0.and.isg==1) then
!--if iis=1 generate initial gas with temperature ftmp
!
        do l=1,mnsp
            rem=0.
            if (iis==1) vmp=sqrt(2.*boltz*ftmp/sp(5,l))
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
            do n=1,mnc
                iprob=1
                if (iwf==0) then
                    wmin=1.
                else
                    wmin=(cg(4,n)**3)/rwf
                    if (wmin<1.) wmin=1.
                end if
!--wmin is the minimum weighting factor in the cell
                if (lflx/=0) then
                    ny=(n-1)/ncx+1
                    nx=n-(ny-1)*ncx
!--nx and ny are the cell column and row
                    if ((lflx>0.and.lfly>0).and.(nx<lflx.and.ny<lfly)) iprob=0
                    if ((lflx>0.and.lfly<0).and.(nx<lflx.and.ny>-lfly)) iprob=0
                    if ((lflx<0.and.lfly>0).and.(nx>-lflx.and.ny<lfly)) iprob=0
                    if ((lflx<0.and.lfly<0).and.(nx>-lflx.and.ny>-lfly)) iprob=0
                end if
                if (iprob==1) then
                    a=ffnd*cc(n)*fsp(l)/(fnum*wmin)+rem
!--a is the nunber of simulated molecules of species l in cell n to
!--simulate the required concentrations at a total number density of fnd
                    if (n<mnc) then
                        mm=a
                        rem=(a-mm)
!--the remainder rem is carried forward to the next cell
                    else
                        mm=nint(a)
                    end if
                    if (mm>0) then
                        do m=1,mm
                            if (nm<mnm) then
!--round-off error could have taken nm to mnm+1
                                nm=nm+1
                                if (mnsp>1) ips(nm)=l
                                pp(1,nm)=cg(1,n)+rf(0)*(cg(2,n)-cg(1,n))
                                ncolm=(pp(1,nm)-cg(1,n))*(nscx-.001)/cg(3,n)+1
!--set the random radius from the distribution of eqn (c6)
                                pp(2,nm)=(cg(4,n)**3+rf(0)*(cg(5,n)**3-cg(4,n)**3))**(1.0/3.0)
                                if (iwf==1) then
                                    wf=(pp(2,nm)**3)/rwf
                                    if (wf<1.) wf=1.
                                    if (wmin/wf<rf(0)) then
                                        nm=nm-1
                                        cycle
!--above takes account of the weighting factor variation in the cell
                                    end if
                                end if
                                ir(nm)=nm
                                nrow=(pp(2,nm)-cg(4,n))*(nscy-.001)/cg(6,n)+1
                                ipl(nm)=(n-1)*nscx*nscy+(nrow-1)*nscx+ncolm
!--species, position, and sub-cell number have been set
                                do k=1,3
                                    call rvelc(pv(k,nm),a,vmp)
                                end do
!--velocity components have been set
!--set the rotational energy
                                if (ispr(1,l)>0) call srot(pr(nm),ftmp,ispr(1,l))
                            end if
                        end do
                    end if
                end if
            end do
        end do

        write (*,99008) nm
99008   format (' ',i6,' molecules')
    
    else if (isg==2) then
!--if iis=1 generate initial gas with temperature ftmp

        do l=1,mnsp
            rem=0
            if (iis==1) vmp=sqrt(2.*boltz*ftmp/sp(5,l))
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
            do n=1,mnc
                iprob=1
                if (iwf==0) then
                    wmin=1.
                else
                    wmin=(cg(4,n)**3)/rwf
                    if (wmin<1.) wmin=1.
                end if
!--wmin is the minimum weighting factor in the cell
                if (lflx/=0) then
                    ny=(n-1)/ncx+1
                    nx=n-(ny-1)*ncx
!--nx and ny are the cell column and row
                    if ((lflx>0.and.lfly>0).and.(nx<lflx.and.ny<lfly)) iprob=0
                    if ((lflx>0.and.lfly<0).and.(nx<lflx.and.ny>-lfly)) iprob=0
                    if ((lflx<0.and.lfly>0).and.(nx>-lflx.and.ny<lfly)) iprob=0
                    if ((lflx<0.and.lfly<0).and.(nx>-lflx.and.ny>-lfly)) iprob=0
                end if
                if (iprob==1) then
                    ymid=0.5*(cg(4,n)+cg(5,n))
                    ffnd=3.219e+24*exp(7056.37*(ymid*ymid-0.0036))
                    a=ffnd*cc(n)*fsp(l)/(fnum*wmin)+rem
!--a is the nunber of simulated molecules of species l in cell n to
!--simulate the required concentrations at a total number density of fnd
                    if (n<mnc) then
                        mm=a
                        rem=(a-mm)
!--the remainder rem is carried forward to the next cell
                    else
                        mm=nint(a)
                    end if
                    if (mm>0) then
                        do m=1,mm
                            if (nm<mnm) then
!--round-off error could have taken nm to mnm+1
                                nm=nm+1
                                if (mnsp>1) ips(nm)=l
                                pp(1,nm)=cg(1,n)+rf(0)*(cg(2,n)-cg(1,n))
                                ncolm=(pp(1,nm)-cg(1,n))*(nscx-.001)/cg(3,n)+1
!--set the random radius from the distribution of eqn (c6)
                                pp(2,nm)=(cg(4,n)**3+rf(0)*(cg(5,n)**3-cg(4,n)**3))**(1.0/3.0)
                                if (iwf==1) then
                                    wf=(pp(2,nm)**3)/rwf
                                    if (wf<1.) wf=1.
                                    if (wmin/wf<rf(0)) then
                                        nm=nm-1
                                        cycle
!--above takes account of the weighting factor variation in the cell
                                    end if
                                end if
                                ir(nm)=nm
                                nrow=(pp(2,nm)-cg(4,n))*(nscy-.001)/cg(6,n)+1
                                ipl(nm)=(n-1)*nscx*nscy+(nrow-1)*nscx+ncolm
!--species, position, and sub-cell number have been set
                                do k=1,3
                                    call rvelc(pv(k,nm),a,vmp)
                                end do
                                pv(3,nm)=pv(3,nm)+10000.*pp(2,nm)
!--velocity components have been set
!--set the rotational energy
                                if (ispr(1,l)>0) call srot(pr(nm),ftmp,ispr(1,l))
                            end if
                        end do
                    end if
                end if
            end do
        end do

        write (*,99001) nm
99001   format (' ',i6,' molecules')
    end if

    if (iis>0) then

!--calculate the number of molecules that enter at each time step
!--across the four sides of the simulated region
        open (6,file='cfdresults',status='old',form='formatted')
        read (6,*) 
        do k=1,231
            read (6,*) tempz(k),temprou(k),tempu(k),tempv(k),tempw(k),tempt(k)
            tempvar=tempz(k)
            tempz(k)=0.24-tempvar
            tempvar=tempu(k)
            tempu(k)=-tempvar
        end do
        close (6)
        
        do j=1,ncx
            do k=1,230
                if ((cg(1,j)+cg(2,j))/2.0<tempz(k).and.(cg(1,j)+cg(2,j))/2.0 >= tempz(k+1)) then
                    vefu(j)=tempu(k)+(tempu(k+1)-tempu(k))/(tempz(k+1)-tempz(k))*((cg(1,j)+cg(2,j))/2.0-tempz(k))
                    vefv(j)=tempv(k)+(tempv(k+1)-tempv(k))/(tempz(k+1)-tempz(k))*((cg(1,j)+cg(2,j))/2.0-tempz(k))
                    vefw(j)=tempw(k)+(tempw(k+1)-tempw(k))/(tempz(k+1)-tempz(k))*((cg(1,j)+cg(2,j))/2.0-tempz(k))
                    rou_bound(j)=temprou(k)+(temprou(k+1)-temprou(k))/(tempz(k+1)-tempz(k))*((cg(1,j)+cg(2,j))/2.0-tempz(k))
                    t_bound(j)=tempt(k)+(tempt(k+1)-tempt(k))/(tempz(k+1)-tempz(k))*((cg(1,j)+cg(2,j))/2.0-tempz(k))
                end if
            end do
        end do
        
        do n=1,4
            if (ib(n)==1) then
!--molecules enter from an external stream
                do l=1,mnsp
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
                    if (n<3) ncs=ncy
                    if (n>2) ncs=ncx
                    do nc=1,ncs
                        a=1/(2.*spi)
!--a is the non-dimensional flux of eqn (4.22)
                        if (n==1.or.n==2) then
                            vmp=sqrt(2.*boltz*t_bound(nc)/sp(5,l))
                            mc=(nc-1)*ncx+1
                            ame(n,l,nc)=rou_bound(nc)/sp(5,l)*fsp(l)*a*vmp*dtm*pi*(cg(5,mc)**2-cg(4,mc)**2)/fnum
                        else
                            if (n==3) then
                                vmp=sqrt(2.*boltz*t_bound(nc)/sp(5,l))
                                ame(n,l,nc)=rou_bound(nc)/sp(5,l)*fsp(l)*a*vmp*dtm*2.*pi*cg(3,nc)*cb(3)/fnum
                            end if
                            if (n==4) then
                                vmp=sqrt(2.*boltz*t_bound(nc)/sp(5,l))
                                ame(n,l,nc)=rou_bound(nc)/sp(5,l)*fsp(l)*a*vmp*dtm*2.*pi*cg(3,nc)*cb(4)/fnum
                            end if
                        end if
                    end do
                end do
            end if
        end do
    end if
!--now calculate the number that enter in jet
    if (ijet>0) then
!--molecules enter from a jet
        do l=1,mnsp
            vmp=sqrt(2.*boltz*tmpj/sp(5,l))
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
            sc=fvj/vmp
!--sc is the inward directed speed ratio
            if (abs(sc)<10.1) a=(exp(-sc*sc)+spi*sc*(1.+erf(sc)))/(2.*spi)
            if (sc>10.) a=sc
            if (sc<-10.) a=0.
!--a is the non-dimensional flux of eqn (4.22)
            ncs=limj(3)-limj(2)+1
            do nc=1,ncs
                ncl=nc+limj(2)-1
                if (ijet==1.or.ijet==2) then
                    mc=(limj(1)-1)*ncx+1
                    wj=2.*pi*cg(3,ncl)*cg(5,mc)
                else
                    mc=(ncl-1)*ncx+1
                    wj=pi*(cg(5,mc)**2-cg(4,mc)**2)
!--wj is the cross-sectional area of the jet in this element
                end if
                amej(l,nc)=fndj*fspj(l)*a*vmp*dtm*wj/fnum
            end do
        end do
    end if
    deallocate(tempz,tempu,tempv,tempw,temprou,tempt)
end subroutine initjet
!movejet.for

subroutine movejet

!--the nm molecules are moved over the time interval dtm

    use mols2
    use molsr
    use cell2
    use gas
    use gasr
    use sampjet
    use samps
    use comp
    use geomjet
    implicit none	

    integer :: flag,n,ift,msc,mc,ks,l1,l2,l3,mcs,num,ni,mcx,mcy,mscx,mscy,k
    real :: at,xi,yi,dx,dy,dz,x,yd,y,ys,a,b,c,s1,s2,s,xc,xsu,xsd,wf,xs,ycd,zc,yc,ysu,ysd,a1,wfi,xd
    real,external :: rf

    ift=-1
!--a negative ift indicates that molecules have not entered at this step
    n=0
    do
        flag=0
        n=n+1
        if (n<=nm) then
            if (ir(n)<0) cycle
!--a duplicated molecule that has already moved has a negative ir
            if (ift<0) at=dtm
            if (ift>0) at=rf(0)*dtm
!--the time step is a random fraction of dtm for entering molecules
            do
                flag=0
                movt=movt+1
                msc=ipl(n)
                if (msc<=0.or.msc>mnsc) call remove(n)
                mc=isc(msc)
!--mc is the initial cell number
                xi=pp(1,n)
                if ((xi+0.00001*cg(3,1))<cb(1).or.(xi-0.00001*cg(3,mnc))>cb(2)) then
                    write (*,*) ' mol ',n,' x coord outside flow ',xi
                    call remove(n)
                    flag=2
                    exit
                end if
                yi=pp(2,n)
                if ((yi+0.00001*cg(6,1))<cb(3).or.(yi-0.00001*cg(6,mnc))>cb(4)) then
                    write (*,*) ' mol ',n,' y coord outside flow ',yi
                    call remove(n)
                    flag=2
                    exit
                end if
                dx=pv(1,n)*at
                dy=pv(2,n)*at
                dz=pv(3,n)*at
                x=xi+dx
                yd=yi+dy
                y=sqrt(yd*yd+dz*dz)
                do ks=1,4
!--check the surfaces
                    if (isurf(ks)>0) then
                        if (isurf(ks)==1.or.isurf(ks)==2) then
                            l1=lims(ks,1)
                            if (l1<=ncy) then
                                ys=cg(4,(l1-1)*ncx+1)
                            else
                                ys=cb(4)
                                l1=l1-1
                            end if
                            if ((isurf(ks)==1.and.(yi>ys.and.yd<ys)).or.(isurf(ks)==2.and.(yi<ys.and.y>ys))) then
                                a=dy**2+dz**2
                                b=yi*dy/a
                                c=b*b-(yi*yi-ys*ys)/a
                                if (c>0.) then
                                    c=sqrt(c)
                                    s1=-b+c
                                    s2=-b-c
!--s1 and s2 are the trajectory fractions to the intersection points
!--the mol. collides with the cyl. if there is a value between 0 and 1
                                    if (s2<0.) then
                                        if (s1>0.) then
                                            s=s1
                                        else
                                            s=2.
                                        end if
                                    else if (s1<s2) then
                                        s=s1
                                    else
                                        s=s2
                                    end if
                                else
                                    s=2.
!--setting s to 2 indicates that there is no intersection
                                end if
!--s is the least positive solution
                                if (s<1.) then
                                    xc=xi+s*dx
                                    if (xc<=cb(1).and.ib(1)==2) then
                                        xc=2.*cb(1)-xc
                                        pv(1,n)=-pv(1,n)
                                    end if
                                    if (xc>=cb(2).and.ib(2)==2) then
                                        xc=2.*cb(2)-xc
                                        pv(1,n)=-pv(1,n)
                                    end if
                                    l2=lims(ks,2)
                                    l3=lims(ks,3)
                                    xsu=cg(1,l2)
                                    xsd=cg(2,l3)
                                    if (xc>xsu.and.xc<xsd) then
!--molecule collides with surface at xc
                                        if (ifcx==0) then
                                            mc=(xc-cb(1))/cw+0.99999
                                        else
                                            xd=(xc-cb(1))/fw+1.e-6
                                            mc=1.+(log(1.-xd*apx))/rpx
!--the cell number is calculated from eqn (12.1)
                                        end if
                                        if (mc<1) mc=1
                                        if (mc>ncx) mc=ncx
                                        mcs=mc-(l2-1)
                                        if (isurf(ks)==1) mc=mc+(l1-1)*ncx
                                        if (isurf(ks)==2) mc=mc+(l1-2)*ncx
!--mc is the cell number for the reflected molecule
                                        if (ks>1) then
                                            do num=2,ks
                                                mcs=mcs+lims(num-1,3)-lims(num-1,2)+1
!--mcs is the code number of the surface element
                                            end do
                                        end if
                                        at=at*(x-xc)/dx
                                        call aifr(yi,dy*s,dz*s,ys,pv(2,n),pv(3,n))
                                        call reflectjet(n,ks,mcs,xc,ys,mc,yi)
                                        if (iwf==1) then
                                            if ((yi**3)>rwf.or.(ys**3)>rwf) then
                                                wfi=(yi**3)/rwf
                                                if (wfi<1.) wfi=1.
                                                wf=(ys**3)/rwf
                                                if (wf<1.) wf=1.
                                                ni=n
                                                call weight(n,wfi,wf)
                                                if (n<ni) then
                                                    flag=2
                                                    exit
                                                end if
                                            end if
                                        end if
                                        flag=1
                                        exit
                                    end if
                                end if
                            end if
                        end if
                        if (isurf(ks)==3.or.isurf(ks)==4) then
                            l1=lims(ks,1)
                            if (l1<=ncx) then
                                if (isurf(ks)==3) then
                                    xs=cg(1,l1)
                                else
                                    xs=cg(2,l1)
                                end if
                            else
                                xs=cb(2)
                                l1=l1-1
                            end if
                            if ((isurf(ks)==3.and.(xi>xs.and.x<xs)).or.(isurf(ks)==4.and.(xi<xs.and.x>xs))) then
                                ycd=yi+(xs-xi)*dy/dx
                                zc=(xs-xi)*dz/dx
                                yc=sqrt(ycd**2+zc**2)
                                l2=lims(ks,2)
                                l3=lims(ks,3)
                                ysu=cg(4,(l2-1)*ncx+1)
                                ysd=cg(5,(l3-1)*ncx+1)
                                if (yc>ysu.and.yc<ysd) then
!--molecule collides with surface at yc
                                    if (ifcy==0) then
                                        mc=(yc-cb(3))/ch+0.99999
                                    else
                                        yd=(yc-cb(3))/fh+1.e-6
                                        mc=1.+(log(1.-yd*apy))/rpy
!--the cell number is calculated from eqn (12.1)
                                    end if
                                    if (mc<1) mc=1
                                    if (mc>ncy) mc=ncy
                                    mcs=mc-(l2-1)
                                    if (isurf(ks)==3) mc=(mc-1)*ncx+l1
                                    if (isurf(ks)==4) mc=(mc-1)*ncx+l1-1
!--mc is the cell number for the reflected molecule
                                    if (ks>1) then
                                        do num=2,ks
                                            mcs=mcs+lims(num-1,3)-lims(num-1,2)+1
!--mcs is the code number of the surface element
                                        end do
                                    end if
                                    a1=at*(xs-xi)/dx
                                    at=at*(x-xs)/dx
                                    call aifr(yi,pv(2,n)*a1,pv(3,n)*a1,yc,pv(2,n),pv(3,n))
                                    call reflectjet(n,ks,mcs,xs,yc,mc,yi)
                                    if (iwf==1) then
                                        if ((yi**3)>rwf.or.(yc**3)>rwf) then
                                            wfi=(yi**3)/rwf
                                            if (wfi<1.) wfi=1.
                                            wf=(yc**3)/rwf
                                            if (wf<1.) wf=1.
                                            ni=n
                                            call weight(n,wfi,wf)
                                            if (n<ni) then
                                                flag=2
                                                exit
                                            end if
                                        end if
                                    end if
                                    flag=1
                                    exit
                                end if
                            end if
                        end if
                    end if
                end do
                if (flag/=1) exit
            end do
            if (flag==2) cycle
            call aifr(yi,dy,dz,y,pv(2,n),pv(3,n))
            if (x<cb(1).or.x>cb(2)) then
                if (x<cb(1)) k=1
                if (x>cb(2)) k=2
!--intersection with boundary k
                if (ib(k)==2) then
!--specular reflection from the boundary (eqn (11.7))
                    x=2.*cb(k)-x
                    pv(1,n)=-pv(1,n)
                else
!--molecule leaves flow
                    call remove(n)
                    cycle
                end if
            end if
            if (y<cb(3).or.y>cb(4)) then
                if (y<cb(3)) k=3
                if (y>cb(4)) k=4
!--intersection with boundary k
                if (ib(k)==2) then
!--specular reflection from the boundary is not allowed
                    write (*,*) ' curved boundaries cannot be a plane of symmetry'
                    stop
                else
!--molecule leaves flow
                    call remove(n)
                    cycle
                end if
            end if

            if (x<cg(1,mc).or.x>cg(2,mc).or.y<cg(4,mc).or.y>cg(5,mc)) then
!--the molecule has moved from the initial cell
                if (ifcx==0) then
                    mcx=(x-cb(1))/cw+0.99999
                else
                    xd=(x-cb(1))/fw+1.e-6
                    mcx=1.+(log(1.-xd*apx))/rpx
!--the cell number is calculated from eqn (12.1)
                end if
                if (mcx<1) mcx=1
                if (mcx>ncx) mcx=ncx
!--mcx is the new cell column (note avoidance of round-off error)
                if (ifcy==0) then
                    mcy=(y-cb(3))/ch+0.99999
                else
                    yd=(y-cb(3))/fh+1.e-6
                    mcy=1.+(log(1.-yd*apy))/rpy
!--the cell number is calculated from eqn (12.1)
                end if
                if (mcy<1) mcy=1
                if (mcy>ncy) mcy=ncy
!--mcy is the new cell row (note avoidance of round-off error)
                mc=(mcy-1)*ncx+mcx
            end if
            mscx=((x-cg(1,mc))/cg(3,mc))*(nscx-.001)+1
            mscy=((y-cg(4,mc))/cg(6,mc))*(nscy-.001)+1
            msc=(mscy-1)*nscx+mscx+nscx*nscy*(mc-1)
!--msc is the new sub-cell number
            if (msc<1) msc=1
            if (msc>mnsc) msc=mnsc
            ipl(n)=msc
            pp(1,n)=x
            pp(2,n)=y
            if (iwf==1) then
                if ((yi**3)>rwf.or.(y**3)>rwf) then
                    wfi=(yi**3)/rwf
                    if (wfi<1.) wfi=1.
                    wf=(y**3)/rwf
                    if (wf<1.) wf=1.
                    call weight(n,wfi,wf)
                end if
            end if
            cycle
        else if (ift<0) then
            ift=1
!--new molecules enter
            call enterjet
            n=n-1
            cycle
        end if
        exit
    end do
end subroutine movejet
!enterjet.for

subroutine enterjet

!--new molecules enter at boundaries

    use mols2
    use molsr
    use cell2
    use gas
    use gasr
    use sampjet
    use comp
    use geomjet
    use const2
    implicit none

    integer :: n,ncs,nc,l,m,k,mc,mscx,mscy,msc,ncl
    real :: wmin,vmp,a,wf,sc,fs1,fs2,qa,u,un,xj,yj
    real,external :: rf

    do n=1,4
!--consider each boundary in turn
        if (ib(n)==1) then
            if (n<3) ncs=ncy
            if (n>2) ncs=ncx
            do nc=1,ncs
                if (iwf==0) then
                    wmin=1.
                else if (n<3) then
                    mc=(nc-1)*ncx+1
                    wmin=(cg(4,mc)**3)/rwf
                    if (wmin<1.) wmin=1.
                else
                    wmin=(cb(n)**3)/rwf
                    if (wmin<1.) wmin=1.
                end if
                if (lflx/=0) then
!--bypass entry into the excluded region of the flow
                    if (n==1) then
                        if (lfly>0.and.lflx>0.and.nc<lfly) cycle
                        if (lfly<0.and.lflx>0.and.nc>lfly) cycle
                    end if
                    if (n==2) then
                        if (lfly>0.and.lflx<0.and.nc<lfly) cycle
                        if (lfly<0.and.lflx<0.and.nc>lfly) cycle
                    end if
                    if (n==3) then
                        if (lflx>0.and.lfly>0.and.nc<lflx) cycle
                        if (lflx<0.and.lfly>0.and.nc>lflx) cycle
                    end if
                    if (n==4) then
                        if (lflx>0.and.lfly<0.and.nc<lflx) cycle
                        if (lflx<0.and.lfly<0.and.nc>lflx) cycle
                    end if
                end if
                do l=1,mnsp
!--consider each species in turn
                    vmp=sqrt(2.*boltz*t_bound(nc)/sp(5,l))
                    a=ame(n,l,nc)/wmin+amr(n,l,nc)
                    m=a
                    amr(n,l,nc)=a-m
!--m molecules enter, remainder has been reset
                    if (m>0) then
                        do k=1,m
                            if (nm<mnm) then
                                nm=nm+1
!--nm is now the number of the new molecule
                                if (n==1) pv(1,nm)=sqrt(-log(rf(0)))*vmp
                                if (n==2) pv(1,nm)=-sqrt(-log(rf(0)))*vmp
                                if (n==3) pv(2,nm)=sqrt(-log(rf(0)))*vmp
                                if (n==4) pv(2,nm)=-sqrt(-log(rf(0)))*vmp
!--for a stationary external gas, use eqn (12.3)
                                if (n<3) call rvelc(pv(2,nm),pv(3,nm),vmp)
                                if (n>2) then
                                    call rvelc(pv(1,nm),pv(3,nm),vmp)
                                    pv(1,nm)=pv(1,nm)+vefu(nc)
                                    pv(2,nm)=pv(2,nm)+vefv(nc)
                                    pv(3,nm)=pv(3,nm)+vefw(nc)
                                end if
!--a single call of rvelc generates the two normal velocity components
                                if (ispr(1,l)>0) call srot(pr(nm),t_bound(nc),ispr(1,l))
                                if (n==1) pp(1,nm)=cb(1)+0.001*cg(3,1)
                                if (n==2) pp(1,nm)=cb(2)-0.001*cg(3,mnc)
                                if (n==3) pp(2,nm)=cb(3)+0.001*cg(6,1)
                                if (n==4) pp(2,nm)=cb(4)-0.001*cg(6,mnc)
!--the molecule is moved just off the boundary
                                if (mnsp>1) ips(nm)=l
                                if (n<3) then
                                    if (n==1) mc=(nc-1)*ncx+1
                                    if (n==2) mc=nc*ncx
                                    pp(2,nm)=(cg(4,mc)**3+rf(0)*(cg(5,mc)**3-cg(4,mc)**3))**(1.0/3.0)
!--this case employs eqn (c6) for the selection of radius
                                end if
                                if (n>2) then
                                    if (n==3) mc=nc
                                    if (n==4) mc=(ncy-1)*ncx+nc
                                    pp(1,nm)=cg(1,mc)+rf(0)*cg(3,mc)
                                end if
                                ir(nm)=nm
                                if (iwf==1) then
                                    wf=(pp(2,nm)**3)/rwf
                                    if (wf<1.) wf=1.
                                    if (wmin/wf<rf(0)) then
                                        nm=nm-1
                                        cycle
!--above takes account of the weighting factor variation in the cell
                                    end if
                                end if
                                mscx=((pp(1,nm)-cg(1,mc))/cg(3,mc))*(nscx-.001)+1
                                mscy=((pp(2,nm)-cg(4,mc))/cg(6,mc))*(nscy-.001)+1
                                msc=(mscy-1)*nscx+mscx+nscx*nscy*(mc-1)
!--msc is the new sub-cell number
                                if (msc<1) msc=1
                                if (msc>mnsc) msc=mnsc
                                ipl(nm)=msc
                            else
                                write (*,*) ' warning: excess molecule limit - restart with an increased fnum'
                            end if
                        end do
                    end if
                end do
            end do
        end if
    end do
!--now the jet molecules
    if (ijet>0) then
        ncs=limj(3)-limj(2)+1
        do nc=1,ncs
            do l=1,mnsp
!--consider each species in turn
                vmp=sqrt(2.*boltz*tmpj/sp(5,l))
                ncl=nc+limj(2)-1
                if (iwf==1) then
                    wmin=(cg(4,ncl)**3)/rwf
                    if (wmin<1.) wmin=1.
                else
                    wmin=1.
                end if
                a=amej(l,nc)/wmin+amrj(l,nc)
                m=a
                amrj(l,nc)=a-m
!--m molecules enter, remainder has been reset
                if (m>0) then
                    if (abs(fvj)>1.e-6) sc=fvj/vmp
                    fs1=sc+sqrt(sc*sc+2.)
                    fs2=0.5*(1.+sc*(2.*sc-fs1))
!the above constants are required for the entering distn. of eqn (12.5)
                    do k=1,m
                        if (nm<mnm) then
                            nm=nm+1
!--nm is now the number of the new molecule
                            if (abs(fvj)>1.e-6) then
                                qa=3.
                                if (sc<-3.) qa=abs(sc)+1.
                                do
                                    u=-qa+2.*qa*rf(0)
!--u is a potential normalised thermal velocity component
                                    un=u+sc
!--un is a potential inward velocity component
                                    if (un<0.) cycle
                                    a=(2.*un/fs1)*exp(fs2-u*u)
                                    if (a<rf(0)) cycle
                                    exit
                                end do
!--the inward normalised vel. component has been selected (eqn (12.5))
                                if (ijet==1) pv(2,nm)=un*vmp
                                if (ijet==2) pv(2,nm)=-un*vmp
                                if (ijet==3) pv(1,nm)=un*vmp
                                if (ijet==4) pv(1,nm)=-un*vmp
                            else
                                if (ijet==1) pv(2,nm)=sqrt(-log(rf(0)))*vmp
                                if (ijet==2) pv(2,nm)=-sqrt(-log(rf(0)))*vmp
                                if (ijet==3) pv(1,nm)=sqrt(-log(rf(0)))*vmp
                                if (ijet==4) pv(1,nm)=-sqrt(-log(rf(0)))*vmp
!--for a stationary external gas, use eqn (12.3)
                            end if
                            if (ijet<3) then
                                call rvelc(pv(1,nm),pv(3,nm),vmp)
                                pv(1,nm)=pv(1,nm)+fuj;
                                pv(3,nm)=pv(3,nm)+fwj;
                            end if
                            if (ijet>2) then
                                call rvelc(pv(2,nm),pv(3,nm),vmp)
                                pv(2,nm)=pv(2,nm)+fvj;
                                pv(3,nm)=pv(3,nm)+fwj;
                            end if
!--a single call of rvelc generates the two normal velocity components
                            if (ispr(1,l)>0) call srot(pr(nm),tmpj,ispr(1,l))
                            if (ijet<3) then
                                mc=(limj(1)-1)*ncx+limj(2)-1+nc
                                yj=cg(4,mc)
                                if (ijet==2) mc=mc-ncx
                            end if
                            if (ijet>2) then
                                mc=limj(1)+(limj(2)-1)*ncx+(nc-1)*ncx
                                xj=cg(1,mc)
                                if (ijet==4) mc=mc-1
                            end if
                            if (ijet==1) pp(2,nm)=yj+0.001*cg(6,mc)
                            if (ijet==2) pp(2,nm)=yj-0.001*cg(6,mc)
                            if (ijet==3) pp(1,nm)=xj+0.001*cg(3,mc)
                            if (ijet==4) pp(1,nm)=xj-0.001*cg(3,mc)
!--the molecule is moved just off the boundary
                            if (mnsp>1) ips(nm)=l
                            ir(nm)=nm
                            if (ijet<3) pp(1,nm)=cg(1,mc)+rf(0)*cg(3,mc)
                            if (ijet>2) pp(2,nm)=(cg(4,n)**3+rf(0)*(cg(5,n)**3-cg(4,n)**3))**(1.0/3.0)
                            if (iwf==1) then
                                wf=(pp(2,nm)**3)/rwf
                                if (wf<1.) wf=1.
                                if (wmin/wf<rf(0)) then
                                    nm=nm-1
                                    cycle
!--above takes account of the weighting factor variation in the cell
                                end if
                            end if
                            mscx=((pp(1,nm)-cg(1,mc))/cg(3,mc))*(nscx-.001)+1
                            mscy=((pp(2,nm)-cg(4,mc))/cg(6,mc))*(nscy-.001)+1
                            msc=(mscy-1)*nscx+mscx+nscx*nscy*(mc-1)
!--msc is the new sub-cell number
                            if (msc<1) msc=1
                            if (msc>mnsc) msc=mnsc
                            ipl(nm)=msc
                        else
                            write (*,*) ' warning: excess molecule limit - restart with an increased fnum'
                        end if
                    end do
                end if
            end do
        end do
    end if
end subroutine enterjet
!reflectjet.for
subroutine reflectjet(n,ks,k,xc,yc,mc,yi)

!--reflection of molecule n from surface ks, element k,
!----location xc,yc, cell mc

    use mols2
    use molsr
    use cell2
    use gas
    use gasr
    use sampjet
    use samps
    use comp
    use geomjet
    use const2
    implicit none

    integer :: n,ks,k,mc
    real :: xc,yc,yi
    integer :: l
    real :: wf,vmp,vni,vpi,upi,wpi,ang,alphan,r,th,um,vn,alphat,vp,wp,alphai,om,cth,x,a
    real,external :: rf

    if (iwf==1.and.(yi**3)>rwf) then
        wf=(yi**3)/rwf
    else
        wf=1.
    end if
    if (mnsp>1) then
        l=ips(n)
    else
        l=1
    end if
!--sample the surface properies due to the incident molecules
    css(1,k,l)=css(1,k,l)+wf
    if (isurf(ks)==1) then
        css(2,k,l)=css(2,k,l)-sp(5,l)*wf*pv(2,n)
        css(4,k,l)=css(4,k,l)+sp(5,l)*wf*pv(1,n)
    end if
    if (isurf(ks)==2) then
        css(2,k,l)=css(2,k,l)+sp(5,l)*wf*pv(2,n)
        css(4,k,l)=css(4,k,l)+sp(5,l)*wf*pv(1,n)
    end if
    if (isurf(ks)==3) then
        css(2,k,l)=css(2,k,l)-sp(5,l)*wf*pv(1,n)
        css(4,k,l)=css(4,k,l)+sp(5,l)*wf*pv(2,n)
    end if
    if (isurf(ks)==4) then
        css(2,k,l)=css(2,k,l)+sp(5,l)*wf*pv(1,n)
        css(4,k,l)=css(4,k,l)+sp(5,l)*wf*pv(2,n)
    end if
    css(5,k,l)=css(5,k,l)+0.5*sp(5,l)*wf*(pv(1,n)**2+pv(2,n)**2+pv(3,n)**2)
    if (mnmr>1) css(7,k,l)=css(7,k,l)+pr(n)*wf

    if (tsurf(ks)<0.) then
!--specular reflection
        if (isurf(ks)==1.or.isurf(ks)==2) pv(2,n)=-pv(2,n)
        if (isurf(ks)==3.or.isurf(ks)==4) pv(1,n)=-pv(1,n)
    else if (alpi(ks)<0.) then
!--diffuse reflection
        vmp=sqrt(2.*boltz*tsurf(ks)/sp(5,l))
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
        if (isurf(ks)==1) then
            pv(2,n)=sqrt(-log(rf(0)))*vmp
            call rvelc(pv(1,n),pv(3,n),vmp)
            pv(3,n)=pv(3,n)+wsurf(ks)
        end if
        if (isurf(ks)==2) then
            pv(2,n)=-sqrt(-log(rf(0)))*vmp
            call rvelc(pv(1,n),pv(3,n),vmp)
            pv(3,n)=pv(3,n)+wsurf(ks)
        end if
        if (isurf(ks)==3) then
            pv(1,n)=sqrt(-log(rf(0)))*vmp
            call rvelc(pv(2,n),pv(3,n),vmp)
            pv(3,n)=pv(3,n)+wsurf(ks)*pp(2,n)
        end if
        if (isurf(ks)==4) then
            pv(1,n)=-sqrt(-log(rf(0)))*vmp
            call rvelc(pv(2,n),pv(3,n),vmp)
            pv(3,n)=pv(3,n)+wsurf(ks)*pp(2,n)
        end if
!--the normal velocity component has been generated
!--a single call of rvelc generates the two tangential vel. components
        if (ispr(1,l)>0) call srot(pr(n),tsurf(ks),ispr(1,l))
    else if (alpi(ks)>=0) then
!--cercignani-lampis-lord reflection model
        vmp=sqrt(2.*boltz*tsurf(ks)/sp(5,l))
!--vmp is the most probable speed in species l, see eqns (4.1) and (4.7)
        if (isurf(ks)==1.or.isurf(ks)==2) then
            if (isurf(ks)==1) vni=-pv(2,n)/vmp
            if (isurf(ks)==2) vni=pv(2,n)/vmp
            upi=pv(1,n)/vmp
        end if
        if (isurf(ks)==3.or.isurf(ks)==4) then
            if (isurf(ks)==3) vni=-pv(1,n)/vmp
            if (isurf(ks)==4) vni=pv(1,n)/vmp
            upi=pv(2,n)/vmp
        end if
        wpi=pv(3,n)/vmp
        ang=atan2(wpi,upi)
        vpi=sqrt(upi*upi+wpi*wpi)
!--vni is the normalized incident normal vel. component (always +ve)
!--vpi is the normalized incident tangential vel. comp. in int. plane
!--ang is the angle between the interaction plane and the x or y axis

!--first the normal component
        alphan=alpn(ks)
        r=sqrt(-alphan*log(rf(0)))
        th=2.*pi*rf(0)
        um=sqrt(1.-alphan)*vni
        vn=sqrt(r*r+um*um+2.*r*um*cos(th))
!--vn is the normalized magnitude of the reflected normal vel. comp.
!----from eqns (14.3)

!--then the tangential component
        alphat=alpt(ks)*(2.-alpt(ks))
        r=sqrt(-alphat*log(rf(0)))
        th=2.*pi*rf(0)
        um=sqrt(1.-alphat)*vpi
        vp=um+r*cos(th)
        wp=r*sin(th)
!--vp,wp are the normalized reflected tangential vel. components in and
!----normal to the interaction plane, from eqn(14.4) and (14.5)
        if (isurf(ks)==1.or.isurf(ks)==2) then
            if (isurf(ks)==1) pv(2,n)=vn*vmp
            if (isurf(ks)==2) pv(2,n)=-vn*vmp
            pv(1,n)=(vp*cos(ang)-wp*sin(ang))*vmp
        end if
        if (isurf(ks)==3.or.isurf(ks)==4) then
            if (isurf(ks)==3) pv(1,n)=vn*vmp
            if (isurf(ks)==4) pv(1,n)=-vn*vmp
            pv(2,n)=(vp*cos(ang)-wp*sin(ang))*vmp
        end if
        pv(3,n)=(vp*sin(ang)+wp*cos(ang))*vmp
        if (ispr(1,l)>0) then
!--set rotational energy by analogy with normal vel. component
            alphai=alpi(ks)
            om=sqrt(pr(n)*(1.-alphai)/(boltz*tsurf(ks)))
            if (ispr(1,l)==2) then
                r=sqrt(-alphai*log(rf(0)))
                cth=cos(2.*pi*rf(0))
            else
!--for polyatomic case, apply acceptance-rejection based on eqn (14.6)
                do
                    x=4.*rf(0)
                    a=2.7182818*x*x*exp(-x*x)
                    if (a<rf(0)) cycle
                    exit
                end do
                r=sqrt(alphai)*x
                cth=2.*rf(0)-1.
            end if
            pr(n)=boltz*tsurf(ks)*(r*r+om*om+2.*r*om*cth)
        end if
    end if
    if (isurf(ks)==1) then
        pp(1,n)=xc
        pp(2,n)=yc+0.001*cg(6,mc)
    end if
    if (isurf(ks)==2) then
        pp(1,n)=xc
        pp(2,n)=yc-0.001*cg(6,mc)
    end if
    if (isurf(ks)==3) then
        pp(1,n)=xc+0.001*cg(3,mc)
        pp(2,n)=yc
    end if
    if (isurf(ks)==4) then
        pp(1,n)=xc-0.001*cg(3,mc)
        pp(2,n)=yc
    end if
    ipl(n)=(mc-1)*nscx*nscy+1
!--sample the surface properties due to the reflected molecules
    if (isurf(ks)==1) css(3,k,l)=css(3,k,l)+sp(5,l)*wf*pv(2,n)
    if (isurf(ks)==2) css(3,k,l)=css(3,k,l)-sp(5,l)*wf*pv(2,n)
    if (isurf(ks)==3) css(3,k,l)=css(3,k,l)+sp(5,l)*wf*pv(1,n)
    if (isurf(ks)==4) css(3,k,l)=css(3,k,l)-sp(5,l)*wf*pv(1,n)
    if (isurf(ks)==1) css(9,k,l)=css(9,k,l)-sp(5,l)*wf*pv(1,n)
    if (isurf(ks)==2) css(9,k,l)=css(9,k,l)-sp(5,l)*wf*pv(1,n)
    if (isurf(ks)==3) css(9,k,l)=css(9,k,l)-sp(5,l)*wf*pv(2,n)
    if (isurf(ks)==4) css(9,k,l)=css(9,k,l)-sp(5,l)*wf*pv(2,n)
    css(6,k,l)=css(6,k,l)-0.5*sp(5,l)*wf*(pv(1,n)**2+pv(2,n)**2+pv(3,n)**2)
    if (mnmr>1) css(8,k,l)=css(8,k,l)-wf*pr(n)
end subroutine reflectjet
!remove.for
subroutine remove(n)

!--remove molecule n and replace it by molecule nm

    use mols2
    use molsr
    implicit none
    integer :: n,m

    pp(1,n)=pp(1,nm)
    pp(2,n)=pp(2,nm)
    do m=1,3
        pv(m,n)=pv(m,nm)
    end do
    if (mnmr>1) pr(n)=pr(nm)
    ipl(n)=ipl(nm)
    if (mnsp>1) ips(n)=ips(nm)
    nm=nm-1
    n=n-1
end subroutine remove
!weight.for

subroutine weight(n,wfi,wf)

!--weighting action for molecule n with weighting factor change

    use mols2
    use molsr
    use mold
    implicit none
    integer :: n
    real :: wfi,wf
    integer :: ll,j,ntm,m
    real :: a
    real,external :: rf

    a=wfi/wf
    ll=0
    do
        if (a<1.) then
            if (rf(0)<a) ll=ll+1
            if (ll==0) call remove(n)
            ll=ll-1
            if (ll/=0) then
                do j=1,ll
                    if (nm<mnm) then
                        ntm=nmb+1
                        if (nmb>=mnb) then
                            ntm=rf(0)*(mnb-.01)+1
!--a random molecule is read from the duplication delay file
                            nmb=nmb-1
                            nm=nm+1
                            do m=1,2
                                pp(m,nm)=ppb(m,ntm)
                            end do
                            do m=1,3
                                pv(m,nm)=pvb(m,ntm)
                            end do
                            if (mnmr>1) pr(nm)=prb(ntm)
                            ipl(nm)=iplb(ntm)
                            if (mnsp>1) ips(nm)=ipsb(ntm)
                            ir(nm)=irb(ntm)
                        end if
!--a negative ir(nm) flags a duplicated molecule that has already moved
                        if (nmb<mnb) nmb=nmb+1
                        do m=1,2
                            ppb(m,ntm)=pp(m,n)
                        end do
                        do m=1,3
                            pvb(m,ntm)=pv(m,n)
                        end do
                        if (mnmr>1) prb(ntm)=pr(n)
                        iplb(ntm)=ipl(n)
                        if (mnsp>1) ipsb(ntm)=ips(n)
                        irb(ntm)=ir(n)
                    end if
                end do
            end if
            exit
        else
            ll=ll+1
            a=a-1.
        end if
    end do
end subroutine weight
!sampijet.for

subroutine sampijet

!--initialises all the sampling variables

    use sampjet
    use sampr
    use samps
    use comp
    implicit none

    integer :: l,n,m

    nsmp=0
    timi=time
    do l=1,mnsp
        do n=1,mnc
            cs(1,n,l)=1.e-6
            do m=2,9
                cs(m,n,l)=0.
            end do
            csr(n,l)=0.
        end do
        do n=1,mnse
            css(1,n,l)=1.e-6
            do m=2,9
                css(m,n,l)=0.
            end do
        end do
    end do
end subroutine sampijet
!samplejet.for

subroutine samplejet

!--sample the molecules in the flow.

    use mols2
    use molsr
    use cell2
    use sampjet
    use sampr
    use geomjet
    use comp
    implicit none

    integer :: nn,n,l,j,k,m,i,ll
    real :: wf

    nsmp=nsmp+1
    do nn=1,mnsg
        do n=1,mnc
            l=ic(2,n,nn)
            cs(9,n,1)=cs(9,n,1)+l*l
            if (l>0) then
                do j=1,l
                    k=ic(1,n,nn)+j
                    m=ir(k)
                    if (iwf==1.and.(pp(2,m)**3)>rwf) then
                        wf=(pp(2,m)**3)/rwf
                    else
                        wf=1.
                    end if
                    if (mnsp>1) then
                        i=ips(m)
                    else
                        i=1
                    end if
                    cs(1,n,i)=cs(1,n,i)+wf
                    cs(8,n,i)=cs(8,n,i)+1.
                    do ll=1,3
                        cs(ll+1,n,i)=cs(ll+1,n,i)+pv(ll,m)*wf
                        cs(ll+4,n,i)=cs(ll+4,n,i)+pv(ll,m)**2*wf
                    end do
                    if (mnmr>1) csr(n,i)=csr(n,i)+wf*pr(m)
                end do
            end if
        end do
    end do
end subroutine samplejet
!outjet.for

subroutine outjet

!--output a progressive set of results to file dsmcjet.out.

    use mols2
    use molsr
    use cell2
    use gas
    use gasr
    use sampjet
    use sampr
    use samps
    use geomjet
    use comp
    use const2
    implicit none

    real :: vel(3),smu(3),svel(3,mnc),sn,sm,smcc,srdf,sre,tt,trot,dboltz,ss(9),mach
    integer :: ks,nel,nel1,nel2,k,nc,l,m,n,rbegin1,rbegin2
    real :: a,x,y,ar,flm,suu,denn,den,uu,temp,xc,yc,ttx,tty,ttz
	
    dboltz=boltz

    open (4,file='dsmcjet.out',form='formatted')
    open (5,file='dsmc_tec.dat',form='formatted')
    open (7,file='dsmcresults1',form='formatted')
    open (8,file='dsmcresults2',form='formatted')

    write (4,*) ' flow sampled from time ',timi,' to time ',time
    write (4,*) ' collisions:-'
    write (4,99001) ((col(m,l),m=1,mnsp),l=1,mnsp)
99001   format (5f13.0)
    write (4,*) ' total number of samples ',nsmp
    write (4,*) nm,' molecules'
    write (4,*) movt,' total molecular moves'
    write (5,*) 'title     = "dsmc"'
    write (5,*) 'variables = "z","r","density","tr temp","rot temp","ovtemp","u","v","w","mach"'
    write (5,*) 'zone t="zone 1"'
    write (5,*) 'i=',ncx,'j=',ncy,'k=',1,'zonetype=ordered'
    write (5,*) 'datapacking=point'
    write (7,*) ncx,cb(1),cb(2)
    write (8,*) ncx,cb(1),cb(2)
    if (ncol>0) then
        write (4,*) int(selt),' selections ',int(ncol),' collision events, ratio  ',real(ncol/selt)
        if (ncol>0) write (4,*) ' mean collision separation ',real(sept/ncol)
    end if

    write (4,*)
    do ks=1,4
        if (isurf(ks)>0) then
            write (4,*) ' surface ',ks
            write (4,*)
            nel=lims(ks,3)-lims(ks,2)+1
            if (ks==1) then
                nel1=1
                nel2=nel
            else
                nel1=nel2+1
                nel2=nel1+nel-1
            end if
            a=fnum/(time-timi)
            write (4,*) '   x coord     y coord    sample    fraction species 1   fraction species 2....'
            do k=nel1,nel2
                if (isurf(ks)<3) then
                    if (lims(ks,1)<=ncy) then
                        nc=(lims(ks,1)-1)*ncx+1
                        y=cg(4,nc)
                    else
                        y=cb(4)
                    end if
                    nc=lims(ks,2)+k-nel1
                    x=0.5*(cg(1,nc)+cg(2,nc))
                end if
                if (isurf(ks)>2) then
                    if (lims(ks,1)<=ncx) then
                        x=cg(1,lims(ks,1))
                    else
                        x=cb(2)
                    end if
                    nc=(lims(ks,2)+k-nel1-1)*ncx+1
                    y=0.5*(cg(4,nc)+cg(5,nc))
                end if
                ss(1)=0.
                do l=1,mnsp
                    ss(1)=ss(1)+css(1,k,l)
                end do
                write (4,99002) x,y,ss(1),(css(1,k,l)/ss(1),l=1,mnsp)
99002           format (2f12.5,f12.1,6f12.6)
            end do

            write (4,*) '   x coord    y coord    num flux   inc press  refl press    incsh str   refl sh str inc tr en  refl tr en  inc rot en  refl rot en net heat flux'
            do k=nel1,nel2
                if (isurf(ks)<3) then
                    if (lims(ks,1)<=ncy) then
                        nc=(lims(ks,1)-1)*ncx+1
                        y=cg(4,nc)
                    else
                        y=cb(4)
                    end if
                    nc=lims(ks,2)+k-nel1
                    x=0.5*(cg(1,nc)+cg(2,nc))
                    ar=2.*pi*cg(3,nc)
                end if
                if (isurf(ks)>2) then
                    if (lims(ks,1)<=ncx) then
                        if (isurf(ks)==3) then
                            x=cg(1,lims(ks,1))
                        else
                            x=cg(2,lims(ks,1))
                        end if
                    else
                        x=cb(2)
                    end if
                    nc=(lims(ks,2)+k-nel1-1)*ncx+1
                    y=0.5*(cg(4,nc)+cg(5,nc))
                    ar=pi*(cg(5,nc)**2-cg(4,nc)**2)
                end if
                do n=1,9
                    ss(n)=0.
                    do l=1,mnsp
                        ss(n)=ss(n)+css(n,k,l)
                    end do
                end do
                do n=1,9
                    ss(n)=ss(n)*a/ar
                end do
                write (4,99003) x,y,(ss(n),n=1,4),ss(9),ss(5),ss(6),ss(7),ss(8),ss(5)+ss(6)+ss(7)+ss(8)
99003           format (14e12.5)
            end do
        end if
    end do

    write (4,*) ' flowfield properties '
    write (4,*) 'samples'
    write (4,*) ' cell    mean sq. fl    sample  sp 1     sample sp 2     etc '
    do n=1,mnc
        ss(1)=0.000001
        do l=1,mnsp
            ss(1)=ss(1)+cs(8,n,l)/real(nsmp)
        end do
        flm=(cs(9,n,1)/real(nsmp)-ss(1)*ss(1))/ss(1)
        write (4,99004) n,flm,(cs(8,n,l),l=1,mnsp)
    end do
99004   format (' ',i6,e12.4,5f9.0)

    write (4,*) ' flowfield properties'
    write (4,*) '  cell   x coord   y coord    density    tr temp   rot temp   ovtemp    u         v         w    mach'
!--first the mixture properties
    rbegin1=0
    rbegin2=0
    do n=1,mnc
        a=fnum/(cc(n)*nsmp)
        sn=0.
        sm=0.
        do k=1,3
            smu(k)=0.
        end do
        smcc=0.
        sre=0.
        srdf=0.
        do l=1,mnsp
            sn=sn+cs(1,n,l)
!--sn is the number sum
            sm=sm+sp(5,l)*cs(1,n,l)
!--sm is the sum of molecular masses
            do k=1,3
                smu(k)=smu(k)+sp(5,l)*cs(k+1,n,l)
!--smu(1 to 3) are the sum of mu, mv, mw
            end do
            smcc=smcc+(cs(5,n,l)+cs(6,n,l)+cs(7,n,l))*sp(5,l)
!--smcc is the sum of m(u**2+v**2+w**2)
            sre=sre+csr(n,l)
!--sre is the sum of rotational energy
            srdf=srdf+ispr(1,l)*cs(1,n,l)
!--srdf is the sum of the rotational degrees of freedom
            suu=suu+sp(5,l)*cs(5,n,l)
!--suu is the sum of m*u*u
        end do
        denn=sn*a
!--denn is the number density, see eqn (1.34)
        den=denn*sm/sn
!--den is the density, see eqn (1.42)
        do k=1,3
            vel(k)=smu(k)/sm
            svel(k,n)=vel(k)
        end do
!--vel and svel are the stream velocity components, see eqn (1.43)
        uu=vel(1)**2+vel(2)**2+vel(3)**2
        tt=(smcc-sm*uu)/(3.d00*dboltz*sn)
!--tt is the translational temperature, see eqn (1.51)
        if (srdf>1.e-6) trot=(2.d00/dboltz)*sre/srdf
!--trot is the rotational temperature, see eqn (11.11)
        temp=(3.d00*tt+(srdf/sn)*trot)/(3.+srdf/sn)
        if (temp==0.)  then
            mach=0.
        else
            mach=sqrt(0.03975*uu/temp)
        end if
!--temp is the overall temperature, see eqn (11.12)
        ct(n)=temp
        xc=0.5*(cg(1,n)+cg(2,n))
        yc=0.5*(cg(4,n)+cg(5,n))
!--xc,yc are the x,y coordinates of the midpoint of the cell
        write (4,99005) n,xc,yc,den,tt,trot,temp,vel(1),vel(2),vel(3),mach
        write (5,99007) xc,yc,den,tt,trot,temp,vel(1),vel(2),vel(3),mach
99005   format (' ',i5,2f10.4,1p,e12.4,0p,7f10.4)
99007   format (' ',2f9.4,1p,2e12.4,0p,9f10.4)
        if ((n>5280.and.n<=5760) .and. rbegin1==0) then
            write (7,*) yc
            rbegin1=1
        end if
        if (n>5280.and.n<=5760) then
            write (7,"(6e20.10)") xc,vel(1),vel(2),vel(3),den,temp
        end if
        if ((n>5760.and.n<=6240) .and. rbegin2==0) then
            write (8,*) yc
            rbegin2=1
        end if
        if (n>5760.and.n<=6240) then
            write (8,"(6e20.10)") xc,vel(1),vel(2),vel(3),den,temp
        end if
    end do

    write (4,*)
    do l=1,mnsp
!--now the properties of the separate species
        write (4,*) ' species ',l
        write (4,*) ' cell   x coord    y coord   n dens     density     ttx       tty      t     tz    tr temp   rot temp    temp   u dif vel v dif vel w dif vel '
        do n=1,mnc
            a=fnum/(cc(n)*nsmp)
            denn=cs(1,n,l)*a
!--denn is the partial number density
            den=sp(5,l)*denn
!--den is the partial density, see eqn (1.13)
            do k=1,3
                vel(k)=cs(k+1,n,l)/cs(1,n,l)
!--vel defines the average velocity of the species l molecules
            end do
            uu=vel(1)**2+vel(2)**2+vel(3)**2
            ttx=(sp(5,l)/dboltz)*(cs(5,n,l)/cs(1,n,l)-vel(1)**2)
            tty=(sp(5,l)/dboltz)*(cs(6,n,l)/cs(1,n,l)-vel(2)**2)
            ttz=(sp(5,l)/dboltz)*(cs(7,n,l)/cs(1,n,l)-vel(3)**2)
!--the component temperatures are based on eqn (1.30)
            tt=(sp(5,l)/(3.d00*dboltz))*((cs(5,n,l)+cs(6,n,l)+cs(7,n,l))/cs(1,n,l)-uu)
!--tt is the translational temperature, see eqn (1.29)
            if (ispr(1,l)>0) then
                trot=2.d00*csr(n,l)/(ispr(1,l)*dboltz*cs(1,n,l))
            else
                trot=0.
            end if
!--trot is the rotational temperature, see eqn (11.10)
            temp=(3.d00*tt+ispr(1,l)*trot)/(3.+ispr(1,l))
            do k=1,3
                vel(k)=vel(k)-svel(k,n)
!--vel now defines the diffusion velocity of species l, see eqn (1.45)
            end do
            xc=0.5*(cg(1,n)+cg(2,n))
            yc=0.5*(cg(4,n)+cg(5,n))
            write (4,99006) n,xc,yc,denn,den,ttx,tty,ttz,tt,trot,temp,vel(1),vel(2),vel(3)
99006       format (' ',i5,2f9.4,1p,2e12.4,0p,9f10.4)
        end do
    end do

    close (4)
    close (5)
    close (7)
    close (8)

end subroutine outjet
!srot.for

subroutine srot(pr,temp,idf)
!--selects a typical equuilibrium value of the rotational energy pr at
!----the temperature temp in a gas with idf rotl. deg. of f.

    use const2
    implicit none
    integer :: idf
    real :: pr,temp
    real :: a,erm,b
    real,external :: rf
 
    if (idf==2) then
        pr=-log(rf(0))*boltz*temp
!--for 2 degrees of freedom, the sampling is directly from eqn (11.22)
    else
!--otherwise apply the acceptance-rejection method to eqn (11.23)
        a=0.5*idf-1.
        do
            erm=rf(0)*10.
!--the cut-off internal energy is 10 kt
            b=((erm/a)**a)*exp(a-erm)
            if (b<rf(0)) cycle
            exit
        end do
        pr=erm*boltz*temp
    end if
end subroutine srot
!erf.for

function erf(s)

!--calculates the error function of s

    real :: b,d,c,t,erf
    b=abs(s)
    if (b>4.) then
        d=1.
    else
        c=exp(-b*b)
        t=1./(1.+0.3275911*b)
        d=1.-(0.254829592*t-0.284496736*t*t+1.421413741*t*t*t-1.453152027*t*t*t*t+1.061405429*t*t*t*t*t)*c
    end if
    if (s<0.) d=-d
    erf=d
end function erf
!indexm.for

subroutine indexm

!--the nm molecule numbers are arranged in order of the molecule groups
!--and, within the groups, in order of the cells and, within the cells,
!--in order of the sub-cells

    use mols2
    use cell2
    use gas
    implicit none
    integer :: mm,nn,n,ls,mg,msc,mc,m,l,k

    do mm=1,mnsg
        ig(2,mm)=0
        do nn=1,mnc
            ic(2,nn,mm)=0
        end do
        do nn=1,mnsc
            iscg(2,nn,mm)=0
        end do
    end do
    do n=1,nm
        if (mnsp>1) then
            ls=ips(n)
        else
            ls=1
        end if
        mg=isp(ls)
        ig(2,mg)=ig(2,mg)+1
        msc=ipl(n)
        iscg(2,msc,mg)=iscg(2,msc,mg)+1
        mc=isc(msc)
        ic(2,mc,mg)=ic(2,mc,mg)+1
    end do
!--number in molecule groups in the cells and sub-cells have been counte
    m=0
    do l=1,mnsg
        ig(1,l)=m
!--the (start address -1) has been set for the groups
        m=m+ig(2,l)
    end do
    do l=1,mnsg
        m=ig(1,l)
        do n=1,mnc
            ic(1,n,l)=m
            m=m+ic(2,n,l)
        end do
!--the (start address -1) has been set for the cells
        m=ig(1,l)
        do n=1,mnsc
            iscg(1,n,l)=m
            m=m+iscg(2,n,l)
            iscg(2,n,l)=0
        end do
    end do
!--the (start address -1) has been set for the sub-cells
 
    do n=1,nm
        if (mnsp>1) then
            ls=ips(n)
        else
            ls=1
        end if
        mg=isp(ls)
        msc=ipl(n)
        iscg(2,msc,mg)=iscg(2,msc,mg)+1
        k=iscg(1,msc,mg)+iscg(2,msc,mg)
        ir(k)=n
!--the molecule number n has been set in the cross-reference array
    end do
end subroutine indexm
!selectjet.for

subroutine selectjet
!--selects a potential collision pair and calculates the product of the
!--collision cross-section and relative speed

    use mols2
    use cell2
    use gas
    use const2
    use elast
    implicit none
    integer :: k,natt,msc,nst,nsg,inc
    real,external :: rf

    k=int(rf(0)*(ic(2,n,nn)-0.001))+ic(1,n,nn)+1
    l=ir(k)
!--the first molecule l has been chosen at random from group nn in cell
    natt=0
    do
        natt=natt+1
        msc=ipl(l)
        if ((nn==mm.and.iscg(2,msc,mm)==1).or.(nn/=mm.and.iscg(2,msc,mm)==0)) then
!--if msc has no type mm molecule find the nearest sub-cell with one
            nst=1
            nsg=1
            do
                inc=nsg*nst
                nsg=-nsg
                nst=nst+1
                msc=msc+inc
                if (msc<1.or.msc>mnsc) cycle
                if (isc(msc)/=n.or.iscg(2,msc,mm)<1) cycle
                exit
            end do
        end if
!--the second molecule m is now chosen at random from the group mm
!--molecules that are in the sub-cell msc
        k=int(rf(0)*(iscg(2,msc,mm)-0.001))+iscg(1,msc,mm)+1
        m=ir(k)
        if (l==m) cycle
!--choose a new second molecule if the first is again chosen

        do k=1,3
            vrc(k)=pv(k,l)-pv(k,m)
        end do
!--vrc(1 to 3) are the components of the relative velocity
        vrr=vrc(1)**2+vrc(2)**2+vrc(3)**2
        if (vrr<1.e-6) then
!--attempted collision between identical molecules, this is due to
!----duplication so choose another as long as there is no infinite loop
            if (natt<10) cycle
            vrr=1.e-6
        end if
        exit
    end do
    vr=sqrt(vrr)
!--vr is the relative speed
    if (mnsp>1) then
        ls=ips(l)
        ms=ips(m)
    else
        ls=1
        ms=1
    end if
    cvr=vr*spm(1,ls,ms)*((2.*boltz*spm(2,ls,ms)/(spm(5,ls,ms)*vrr))**(spm(3,ls,ms)-0.5))/spm(6,ls,ms)
!--the collision cross-section is based on eqn (4.63)
end subroutine selectjet
!elastic.for

subroutine elastic

!--generate the post-collision velocity components.

    use mols2
    use gas
    use const2
    use elast
    implicit none

    real :: vrcp(3),vccm(3)
!--vrcp(3) are the post-collision components of the relative velocity
!--vccm(3) are the components of the centre of mass velocity

    integer :: k
    real :: rml,rmm,a,b,c,d,oc,sc
    real,external :: rf

    rml=spm(5,ls,ms)/sp(5,ms)
    rmm=spm(5,ls,ms)/sp(5,ls)
    do k=1,3
        vccm(k)=rml*pv(k,l)+rmm*pv(k,m)
    end do
!--vccm defines the components of the centre-of-mass velocity, eqn (2.1)
    if (abs(spm(4,ls,ms)-1.)<1.e-3) then
!--use the vhs logic
        b=2.*rf(0)-1.
!--b is the cosine of a random elevation angle
        a=sqrt(1.-b*b)
        vrcp(1)=b*vr
        c=2.*pi*rf(0)
!--c is a random azimuth angle
        vrcp(2)=a*cos(c)*vr
        vrcp(3)=a*sin(c)*vr
    else
!--use the vss logic
        b=2.*(rf(0)**spm(4,ls,ms))-1.
!--b is the cosine of the deflection angle for the vss model, eqn (11.8)
        a=sqrt(1.-b*b)
        c=2.*pi*rf(0)
        oc=cos(c)
        sc=sin(c)
        d=sqrt(vrc(2)**2+vrc(3)**2)
        if (d>1.e-6) then
            vrcp(1)=b*vrc(1)+a*sc*d
            vrcp(2)=b*vrc(2)+a*(vr*vrc(3)*oc-vrc(1)*vrc(2)*sc)/d
            vrcp(3)=b*vrc(3)-a*(vr*vrc(2)*oc+vrc(1)*vrc(3)*sc)/d
        else
            vrcp(1)=b*vrc(1)
            vrcp(2)=a*oc*vrc(1)
            vrcp(3)=a*sc*vrc(1)
        end if
!--the post-collision rel. velocity components are based on eqn (2.22)
    end if
!--vrcp(1 to 3) are the components of the post-collision relative vel.
    do k=1,3
        pv(k,l)=vccm(k)+vrcp(k)*rmm
        pv(k,m)=vccm(k)-vrcp(k)*rml
    end do
end subroutine elastic
!rvelc.for

subroutine rvelc(u,v,vmp)

!--generates two random velocity components u an v in an equilibrium
!--gas with most probable speed vmp  (based on eqns (c10) and (c12))

    implicit none
    real :: u,v,vmp
    real :: a,b
    real,external :: rf

    a=sqrt(-log(rf(0)))
    b=6.283185308*rf(0)
    u=a*sin(b)*vmp
    v=a*cos(b)*vmp
end subroutine rvelc
!gam.for

function gam(x)

!--calculates the gamma function of x.

    real :: a,y,gam
    a=1.
    y=x
    if (y<1.) then
        a=a/y
    else
        do
            y=y-1
            if (y>=1.) then
                a=a*y
                cycle
            end if
            exit
        end do
    end if
    gam=a*(1.-0.5748646*y+0.9512363*y**2-0.6998588*y**3+0.4245549*y**4-0.1010678*y**5)
end function gam
!collmr.for

subroutine collmr

!--calculates collisions appropriate to dtm in a gas mixture

    use mols2
    use molsr
    use cell2
    use gas
    use gasr
    use geomjet
    use sampjet
    use sampr
    use comp
    use const2
    use elast
    implicit none

    integer :: k,nsel,isel
    real :: sn,avn,asel,cvm
    real,external :: rf

!--vrc(3) are the pre-collision components of the relative velocity

    do n=1,mnc
!--consider collisions in cell n
        do nn=1,mnsg
            do mm=1,mnsg
                sn=0.
                do k=1,mnsp
                    if (isp(k)==mm) sn=sn+cs(1,n,k)
                end do
                if (sn>1.) then
                    avn=sn/real(nsmp)
                else
                    avn=ic(2,n,mm)
                end if
!--avn is the average number of group mm molecules in the cell
                asel=0.5*ic(2,n,nn)*avn*fnum*ccg(1,n,nn,mm)*dtm/cc(n)+ccg(2,n,nn,mm)
!--asel is the number of pairs to be selected, see eqn (11.5)
                nsel=asel
                ccg(2,n,nn,mm)=asel-nsel
                if (nsel>0) then
                    if (((nn/=mm).and.(ic(2,n,nn)<1.or.ic(2,n,mm)<1)).or.((nn==mm).and.(ic(2,n,nn)<2))) then
                        ccg(2,n,nn,mm)=ccg(2,n,nn,mm)+nsel
!--if there are insufficient molecules to calculate collisions,
!--the number nsel is added to the remainer ccg(2,n,nn,mm)
                    else
                        cvm=ccg(1,n,nn,mm)
                        selt=selt+nsel
                        do isel=1,nsel

                            call selectjet

                            if (cvr>cvm) cvm=cvr
!--if necessary, the maximum product in cvm is upgraded
                            if (rf(0)<cvr/ccg(1,n,nn,mm)) then
!--the collision is accepted with the probability of eqn (11.6)
                                ncol=ncol+1
                                sept=sept+sqrt((pp(1,l)-pp(1,m))**2+(pp(2,l)-pp(2,m))**2)
                                col(ls,ms)=col(ls,ms)+1.d00
                                col(ms,ls)=col(ms,ls)+1.d00

                                if (ispr(1,ls)>0.or.ispr(1,ms)>0) call inelr
!--bypass rotational redistribution if both molecules are monatomic

                                call elastic

                            end if
                        end do
                        ccg(1,n,nn,mm)=cvm
                    end if
                end if
            end do
        end do
    end do
end subroutine collmr
!inelr.for

subroutine inelr

!--adjustment of rotational energy in a collision

    use molsr
    use gas
    use gasr
    use elast
    implicit none

    integer :: nsp,k,ks,js,irt,ir(2)
    real :: eti,eci,ecf,ecc,xib,atk,erm,xia,etf,a
    real,external :: rf

!--ir is the indicator for the rotational redistribution
    eti=0.5*spm(5,ls,ms)*vrr
!--eti is the initial translational energy
    eci=0.
!--eci is the initial energy in the active rotational modes
    ecf=0.
!--ecf is the final energy in these modes
    ecc=eti
!--ecc is the energy to be divided
    xib=2.5-spm(3,ls,ms)
!--xib is th number of modes in the redistribution
    irt=0
!--irt is 0,1 if no,any redistribution is made
    do nsp=1,2
!--consider the molecules in turn
        if (nsp==1) then
            k=l
            ks=ls
            js=ms
        else
            k=m
            ks=ms
            js=ls
        end if
        ir(nsp)=0
        if (ispr(1,ks)>0) then
            if (ispr(2,ks)==0) then
                atk=1./spr(1,ks,js)
            else
                atk=1./(spr(1,ks,js)+spr(2,ks,js)*ct(n)+spr(3,ks,js)*ct(n)**2)
            end if
!--atk is the probability that rotation is redistributed to molecule l
            if (atk>rf(0)) then
                irt=1
                ir(nsp)=1
                if (mnmr>1) then
                    ecc=ecc+pr(k)
                    eci=eci+pr(k)
                end if
                xib=xib+0.5*ispr(1,ks)
            end if
        end if
    end do
!--apply the general larsen-borgnakke distribution function
    if (irt==1) then
        do nsp=1,2
            if (ir(nsp)==1) then
                if (nsp==1) then
                    k=l
                    ks=ls
                else
                    k=m
                    ks=ms
                end if
                xib=xib-0.5*ispr(1,ks)
!--the current molecule is removed from the total modes
                if (ispr(1,ks)==2) then
                    erm=1.-rf(0)**(1./xib)
                else
                    xia=0.5*ispr(1,ks)
                    call lbs(xia-1.,xib-1.,erm)
                end if
                if (mnmr>1) then
                    pr(k)=erm*ecc
                    ecc=ecc-pr(k)
!--the available energy is reduced accordingly
                    ecf=ecf+pr(k)
                end if
            end if
        end do
        etf=eti+eci-ecf
!--etf  is the post-collision translational energy
!--adjust vr and, for the vss model, vrc for the change in energy
        a=sqrt(2.*etf/spm(5,ls,ms))
        if (abs(spm(4,ls,ms)-1.)<1.e-3) then
            vr=a
        else
            do k=1,3
                vrc(k)=vrc(k)*a/vr
            end do
            vr=a
        end if
    end if
end subroutine inelr
!lbs.for

subroutine lbs(xma,xmb,erm)
!--selects a larsen-borgnakke energy ratio using eqn (11.9)
    implicit none
    real :: xma,xmb,erm
    real :: p
    real,external :: rf

    do
        erm=rf(0)
        if (xma<1.e-6.or.xmb<1.e-6) then
            if (xma<1.e-6.and.xmb<1.e-6) exit
            if (xma<1.e-6) p=(1.-erm)**xmb
            if (xmb<1.e-6) p=(1.-erm)**xma
        else
            p=(((xma+xmb)*erm/xma)**xma)*(((xma+xmb)*(1.-erm)/xmb)**xmb)
        end if
        if (p<rf(0)) cycle
        exit
    end do
end subroutine lbs
!aifr.for

subroutine aifr(yi,dy,dz,y,v,w)
!--calculates the new radius and realigns the velocity components in
!----the axially symmetric flow
    implicit none
    real :: yi,dy,dz,y,v,w
    real :: dr,vr,a,s,c,b

    dr=dz
    vr=w
    a=yi+dy
    y=sqrt(a*a+dr*dr)
    s=dr/y
    c=a/y
    b=v
    v=b*c+vr*s
    w=-b*s+vr*c
end subroutine aifr
!rf.for

function rf(idum)
!--generates a uniformly distributed random fraction between 0 and 1
!----idum will generally be 0, but negative values may be used to
!------re-initialize the seed
    integer,save :: ma(55),inext,inextp
    integer,parameter :: mbig=1000000000,mseed=161803398,mz=0
    real,parameter :: fac=1.e-9
    data iff/0/
    integer :: mj,mk,i,ii,k
    real :: rf

    if (idum<0.or.iff==0) then
        iff=1
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(55)=mj
        mk=1
        do i=1,54
            ii=mod(21*i,55)
            ma(ii)=mk
            mk=mj-mk
            if (mk<mz) mk=mk+mbig
            mj=ma(ii)
        end do
        do k=1,4
            do i=1,55
                ma(i)=ma(i)-ma(1+mod(i+30,55))
                if (ma(i)<mz) ma(i)=ma(i)+mbig
            end do
        end do
        inext=0
        inextp=31
    end if
    do
        inext=inext+1
        if (inext==56) inext=1
        inextp=inextp+1
        if (inextp==56) inextp=1
        mj=ma(inext)-ma(inextp)
        if (mj<mz) mj=mj+mbig
        ma(inext)=mj
        rf=mj*fac
        if (rf>1.e-8.and.rf<0.99999999) exit
    end do
end function rf
!datajet.for

subroutine datajet

!--defines the data for a particular run of dsmcjet.for.

    use gas
    use gasr
    use cell2
    use sampjet
    use comp
    use geomjet
    implicit none

!--set data (must be consistent with parameter variables)

    open (2,file='input.dat',status='old',form='formatted')
    read (2,*)
    read (2,*) iwf,rwf,ncx,ncy,nscx,nscy,ifcx,ifcy,cwry,iis,isg,lflx,lfly
    read (2,*)
    read (2,*) ftmp,fnd,ffnd,vfw,fsp(1),fnum,dtm,cb(1),cb(2),cb(3),cb(4)
    read (2,*)
    read (2,*) ib(1),ib(2),ib(3),ib(4),isurf(1),lims(1,1),lims(1,2),lims(1,3),tsurf(1),wsurf(1),alpi(1)
    read (2,*)
    read (2,*) isurf(2),lims(2,1),lims(2,2),lims(2,3),tsurf(2),wsurf(2),alpi(2),isurf(3),lims(3,1),lims(3,2),lims(3,3),tsurf(3),wsurf(3),alpi(3)
    read (2,*)
    read (2,*) isurf(4),lims(4,1),lims(4,2),lims(4,3),tsurf(4),wsurf(4),alpi(4),ijet,limj(1),limj(2),limj(3),tmpj,fndj,fuj,fvj,fwj,fspj(1)
    read (2,*)
    read (2,*) sp(1,1),sp(2,1),sp(3,1),sp(4,1),sp(5,1),ispr(1,1),spr(1,1,1),ispr(2,1),isp(1),nis,nsp,npt
    close (2)
end subroutine datajet