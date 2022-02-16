program conv_int
implicit none
integer, parameter:: dp=kind(0.d0)
integer:: np !numero punti in cui calcolare l'integrale
integer:: i,j!,j !definiscono la posizione nella macromatrice dell'equazione di Dirac in notazione bi-spinoriale
integer:: kp_in_max,kp_fin_max,ke_in_max,ke_fin_max
integer:: k_e_in, k_e_fin, k_p_in, k_p_fin
real(dp):: r_max,dr!, drp,dre
real(dp):: mp,me,hbar,c,pi,dummy(1)
real(dp), external:: cof3j
integer:: nmax !è il massimo n numero quantico principale da prendere per gli autostati, noi prendiamo solo il primo

integer:: l,lmax,m,ie,ip,p,q
real(dp):: somma_m,somma_l, integrale
real(dp):: F_p_up, F_p_down, F_e_up, F_e_down
real(dp):: rag
real(dp):: somma_scal_p,fact_scal_p,somma_scal_e,fact_scal_e,int_scal
real(dp):: int_vett(3), somma_vett_p, fact_vett_p, somma_vett_e, fact_vett_e, int_vett_tot
real(dp):: A_p_up(3), A_p_down(3), A_e_up(3), A_e_down(3)
real(dp):: m_e_in, m_e_fin, m_p_in, m_p_fin




real(dp), allocatable:: autostat_e_in(:),autostat_p_in(:),autostat_e_fin(:),autostat_p_fin(:)
real(dp), allocatable:: autoen_e_in(:),autoen_p_in(:),autoen_e_fin(:),autoen_p_fin(:)
real(dp), allocatable:: work_e_in(:), work_p_in(:), work_e_fin(:), work_p_fin(:)
integer:: lwork_e_in, info_e_in, lwork_e_fin, info_e_fin, lwork_p_in, info_p_in, lwork_p_fin, info_p_fin

real(dp), allocatable:: d_off_e_in(:), d_diag_e_in(:), d_e_in(:)
real(dp), allocatable:: d_off_p_in(:), d_diag_p_in(:), d_p_in(:)
real(dp), allocatable:: d_off_e_fin(:), d_diag_e_fin(:), d_e_fin(:)
real(dp), allocatable:: d_off_p_fin(:), d_diag_p_fin(:), d_p_fin(:)
real(dp), allocatable:: F_e_in(:,:),F_p_in(:,:),F_e_fin(:,:),F_p_fin(:,:)
real(dp), allocatable:: G_e_in(:,:),G_p_in(:,:),G_e_fin(:,:),G_p_fin(:,:)



mp=938.27208816E6            !eV massa protone
me=510998.95                 !eV massa elettrone
hbar= 6.582119569*10**(-16.) !eV*s costante di planck  
c=299792458                  !m/s velocità luce
pi=acos(-1.)

np=10
r_max=1
dr=r_max/np
kp_in_max=1
kp_fin_max=1
ke_in_max=1
ke_fin_max=1

m_e_in=1
m_e_fin=1
m_p_in=1
m_p_fin=1

!stato elettronico iniziale (libero)
allocate(d_e_in(0:4*np*np-1),d_off_e_in(0:4*np*np-1), d_diag_e_in(0:4*np*np-1))
call dirac_off_diag(k_e_in,r_max,np,d_off_e_in)
call dirac_diagonal_lib(me,r_max,np,d_diag_e_in)

do i=0,4*np*np-1
   d_e_in(i)=d_off_e_in(i)+d_diag_e_in(i)
end do

deallocate(d_off_e_in, d_diag_e_in)

!stato protonico iniziale (libero)
allocate(d_p_in(0:4*np*np-1),d_off_p_in(0:4*np*np-1), d_diag_p_in(0:4*np*np-1))
call dirac_off_diag(k_p_in,r_max,np,d_off_p_in)
call dirac_diagonal_lib(mp,r_max,np,d_diag_p_in)

do i=0,4*np*np-1
   d_p_in(i)=d_off_p_in(i)+d_diag_p_in(i)
end do

deallocate(d_off_p_in, d_diag_p_in)


!stato elettronico finale (libero)
allocate(d_e_fin(0:4*np*np-1),d_off_e_fin(0:4*np*np-1), d_diag_e_fin(0:4*np*np-1))
call dirac_off_diag(k_e_fin,r_max,np,d_off_e_fin)
call dirac_diagonal_lib(me,r_max,np,d_diag_e_fin)

do i=0,4*np*np-1
   d_e_fin(i)=d_off_e_fin(i)+d_diag_e_fin(i)
end do

deallocate(d_off_e_fin, d_diag_e_fin)

!stato protonico finale (legato)
allocate(d_p_fin(0:4*np*np-1),d_off_p_fin(0:4*np*np-1), d_diag_p_fin(0:4*np*np-1))
call dirac_off_diag(k_p_fin,r_max,np,d_off_p_fin)
call dirac_diagonal(mp,r_max,np,d_diag_p_fin)

do i=0,4*np*np-1
   d_p_fin(i)=d_off_p_fin(i)+d_diag_p_fin(i)
end do

deallocate(d_off_p_fin, d_diag_p_fin)


!DIAGONALIZZAZIONE

!salvo le matrici di dirac su matrici temporanee che saranno trasformate in array di autovettori
!Le matrici di Dirac vanno da 0 a 4n*n -1.
allocate(autostat_e_in(0:4*np*np-1),autostat_p_in(0:4*np*np-1),autostat_e_fin(0:4*np*np-1),autostat_p_fin(0:4*np*np-1))
do i=1,4*np*np-1
  autostat_e_in(i)=d_e_in(i)
  autostat_p_in(i)=d_p_in(i)
  autostat_e_fin(i)=d_e_fin(i)
  autostat_p_fin(i)=d_p_fin(i)
end do

!alloco array per autovalori
allocate(autoen_e_in(0:2*np-1))!,work_e_in(1:2*np*np))
allocate(autoen_p_in(0:2*np-1))!,work_p_in(1:2*np*np))
allocate(autoen_e_fin(0:2*np-1))!,work_e_fin(1:2*np*np))
allocate(autoen_p_fin(0:2*np-1))!,work_p_fin(1:2*np*np))
!faccio girare dsyev a vuoto per trovare lwork ottimale
lwork_e_in=-1
lwork_p_in=-1
lwork_e_fin=-1
lwork_p_fin=-1
call dsyev('V','U',2*np, d_e_in, 2*np, autostat_e_in , dummy, lwork_e_in, info_e_in ) !l'input 7 della sub vuole per forza un array
lwork_e_in = nint(dummy(1))
call dsyev('V','U',2*np, d_p_in, 2*np, autostat_p_in , dummy, lwork_p_in, info_p_in ) 
lwork_p_in = nint(dummy(1))
call dsyev('V','U',2*np, d_e_fin, 2*np, autostat_e_fin , dummy, lwork_e_fin, info_e_fin ) 
lwork_e_fin = nint(dummy(1))
call dsyev('V','U',2*np, d_p_fin, 2*np, autostat_p_fin , dummy, lwork_p_fin, info_p_fin ) 
lwork_p_fin = nint(dummy(1))

!diagonalizzo
allocate(work_e_in(1:lwork_e_in),work_p_in(1:lwork_p_in),work_e_fin(1:lwork_e_fin),work_p_fin(1:lwork_p_fin))
call dsyev('V',  'U',  2*np,   autostat_e_in,   2*np,   autoen_e_in,   work_e_in,   lwork_e_in,   info_e_in)
call dsyev('V',  'U',  2*np,   autostat_p_in,   2*np,   autoen_p_in,   work_p_in,   lwork_p_in,   info_p_in)
call dsyev('V',  'U',  2*np,   autostat_e_fin,  2*np,   autoen_e_fin,  work_e_fin,  lwork_e_fin,  info_e_fin)
call dsyev('V',  'U',  2*np,   autostat_p_fin,  2*np,   autoen_p_fin,  work_p_fin,  lwork_p_fin,  info_p_fin)

!riorganizzo gli autovettori e divido le componenti large dalle componenti down
!a questo punto sono salvati in autostat(i pari, j)    le F( punto r_i   ,energia j-esima)
!                               autostat(i dispari, j) le G( punto r_i-1 ,energia j-esima)
allocate(F_e_in(0:np-1,0:2*np-1),F_p_in(0:np-1,0:2*np-1),F_e_fin(0:np-1,0:2*np-1),F_p_fin(0:np-1,0:2*np-1))
allocate(G_e_in(0:np-1,0:2*np-1),G_p_in(0:np-1,0:2*np-1),G_e_fin(0:np-1,0:2*np-1),G_p_fin(0:np-1,0:2*np-1))

do i=0,np-1
   do j=0,2*np-1
     F_e_in(i,j)=autostat_e_in(2*i+j*2*np)
     F_p_in(i,j)=autostat_p_in(2*i+j*2*np)
     F_e_fin(i,j)=autostat_e_fin(2*i+j*2*np)
     F_p_fin(i,j)=autostat_p_fin(2*i+j*2*np)
     G_e_in(i,j)=autostat_e_in(2*(i+1)+j*2*np)
     G_p_in(i,j)=autostat_p_in(2*(i+1)+j*2*np)
     G_e_fin(i,j)=autostat_e_fin(2*(i+1)+j*2*np)
     G_p_fin(i,j)=autostat_p_fin(2*(i+1)+j*2*np)
   end do
end do


!INTEGRAZIONE


somma_l=0
do l=0,lmax
  somma_m=0
  do m=-l,l

    !integro l'integrando scalare, le j=0 usano solo lo 0-esimo autostato (la prima colonna degli autostati), va sostituita con una mistura a coeff Boltzmann
    somma_scal_p=0
    fact_scal_p=0
    do ip=0,np-1 !nel punto ip-esimo per la parte protonica
              
       !integro sulla parte elettronica
       somma_scal_e=0
       fact_scal_e=0
       do ie=0,np-1 !nel punto ie-esimo per la parte elettronica 
          call cal_F(k_e_fin,k_e_in,l,m_e_fin,m_e_in,m,F_e_up)
          call cal_F(-k_e_fin,-k_e_in,l,m_e_fin,m_e_in,m,F_e_down)
          fact_scal_e=(F_e_fin(ie,0)*F_e_in(ie,0)*F_e_up+G_e_fin(ie,0)*G_e_in(ie,0)*F_e_down) !sovrascrive, ok    <------------|
          if (ip>ie) then  !switch per il fattore r<^l/r>^(l+1)                                                                |
             rag=(((ie+1)*dr)**l)*(1./(((ip+1)*dr)**(l+1)))  !RICORDARSI DI METTERE IL +1 ALTRIMENTI FA DIVISIONE PER 0        |
          else   !                                                                                                             |  
             rag=(((ip+1)*dr)**l)*(1./(((ie+1)*dr)**(l+1))) !                                                                  |
          end if !                                                                                                             |                                                             
          somma_scal_e=somma_scal_e+rag*fact_scal_e*dr  !_____________________________________________________________________/
       end do
       
       call  cal_F(k_p_fin,k_p_in,l,m_p_fin,m_p_in,m,F_p_up)
       call  cal_F(-k_p_fin,-k_p_in,l,m_p_fin,m_p_in,m,F_p_down)
       fact_scal_p=(F_p_fin(ip,0)*F_p_in(ip,0)*F_p_up+G_p_fin(ip,0)*G_p_in(ip,0)*F_p_down)
       somma_scal_p=somma_scal_p+somma_scal_e*fact_scal_p*dr
    end do 
    int_scal=somma_scal_p

    
    !integro l'integrando vettoriale (p componenti x,y,z=1,2,3), le j=0 usano solo lo 0-esimo autostato (la prima colonna degli autostati), va sostituita con una mistura a coeff Boltzmann
    do p=1,3
       fact_vett_p=0
       somma_vett_p=0
       do ip=0,np-1 !nel punto ip-esimo per la parte protonica
                    
          !integro sulla parte elettronica
          somma_vett_e=0
          fact_vett_e=0
          do ie=0,np-1 !nel punto ie-esimo per la parte elettronica 
             call cal_A(k_e_fin,k_e_in,l,m_e_fin,m_e_in,m,A_e_up(p))
             call cal_A(-k_e_fin,-k_e_in,l,m_e_fin,m_e_in,m,A_e_down(p))
             fact_vett_e=(F_e_fin(ie,0)*F_e_in(ie,0)*A_e_up(p)+G_e_fin(ie,0)*G_e_in(ie,0)*A_e_down(p)) !sovrascrive, ok-----> sostituire con mistura coeff Boltzmann
             if (ip>ie) then                  !switch per il fattore r<^l/r>^(l+1)
               rag=(((ie+1)*dr)**l)*(1./(((ip+1)*dr)**(l+1)))
             else
               rag=(((ip+1)*dr)**l)*(1./(((ie+1)*dr)**(l+1)))
             end if
             somma_vett_e=somma_vett_e+rag*fact_vett_e*dr
          end do
          
          
          call cal_A(k_p_fin,k_p_in,l,m_p_fin,m_p_in,m,A_p_up(p))
          call cal_A(-k_p_fin,-k_p_in,l,m_p_fin,m_p_in,m,A_p_down(p))
          fact_vett_p=(F_p_fin(ip,0)*F_p_in(ip,0)*A_e_up(p)+G_p_fin(ip,0)*G_p_in(ip,0)*A_p_down(p))
          somma_vett_p=somma_vett_p+somma_vett_e*fact_vett_p*ip*dr
       end do 
       int_vett(p)=somma_vett_p
    end do

    int_vett_tot=0
    do p=1,3
       int_vett_tot=int_vett_tot+int_vett(p)
    end do

    integrale=(4*pi/(2*l+1))*(int_scal-int_vett_tot)
    somma_m=somma_m+integrale

  end do !m
  somma_l=somma_l+somma_m

end do

write(*,*) somma_l








deallocate(F_e_in,F_p_in,F_e_fin,F_p_fin,G_e_in,G_p_in,G_e_fin,G_p_fin)
deallocate(autoen_e_in,work_e_in)
deallocate(autoen_p_in,work_p_in)
deallocate(autoen_e_fin,work_e_fin)
deallocate(autoen_p_fin,work_p_fin)
deallocate(autostat_e_in,autostat_p_in,autostat_e_fin,autostat_p_fin)
deallocate(d_e_in,d_p_in,d_e_fin,d_p_fin)
write(*,*) "fine"
end program conv_int












subroutine dirac_off_diag(k,r_max,np,mat)
integer, parameter:: dp=kind(0.d0)
integer:: i,j,np
real(dp):: r_max, dr
real(dp):: hbar,c
real(dp):: mat(0:4*np*np-1)

hbar= 6.582119569*10**(-16.) !eV*s costante di planck  
c=299792458         !m/s

dr=r_max/np

do i=0,2*np-1
  do j=0,2*np-1
    if (i==j-1) then
       
       if (mod(i,2)==0) then     ! riga F, pari
          mat(i+j*2*np)=hbar*c*k
       else                      ! riga G, dispari
          mat(i+j*2*np)=hbar*c*(0.5/dr)
       end if
       
    else if (i==j+1) then
       
       if (mod(i,2)==0) then     ! riga F, pari
          mat(i+j*2*np)=hbar*c*(0.5/dr)
       else                      ! riga G, dispari
          mat(i+j*2*np)=hbar*c*k
       end if
       
    else if (i==j-3 .and. mod(i,2)==0) then
         mat(i+j*2*np)=-hbar*c*(0.5/dr)
        
    else if (i==j+3 .and. mod(i,2)==1) then
         mat(i+j*2*np)=-hbar*c*(0.5/dr)
       
    else
      mat(i+j*2*np)=0
    end if
    
  end do
end do
end subroutine dirac_off_diag

subroutine dirac_diagonal_lib(m,r_max,np,mat)
integer, parameter:: dp=kind(0.d0)
integer:: i,j,np
real(dp):: mat(0:4*np*np-1)
real(dp):: r_max,dr,c,m

c=299792458         !m/s
dr=r_max/np
do i=0,2*np-1
  do j=0,2*np-1
    
    if (i==j) then
      
      if (mod(i,2)==0) then
        mat(i+j*2*np)=m*c*c
      else
        mat(i+j*2*np)=-m*c*c
      end if
    
    else
      mat(i+j*2*np)=0
    
    end if  
  
  !write(*,*) mat(i+j*2*np)
  end do
end do


end subroutine dirac_diagonal_lib


subroutine dirac_diagonal(m,r_max,np,mat)
integer, parameter:: dp=kind(0.d0)
integer:: i,j,np
real(dp):: mat(0:4*np*np-1)
real(dp):: r_max,dr,c,m,ws

c=299792458         !m/s
dr=r_max/np
do i=0,2*np-1
  do j=0,2*np-1
    
    if (i==j) then
      
      if (mod(i,2)==0) then
        mat(i+j*2*np)=m*c*c+ws((i+1)*dr)
      else
        mat(i+j*2*np)=-m*c*c+ws((i+1)*dr)
      end if
    
    else
      mat(i+j*2*np)=0
    
    end if  
  
  !write(*,*) mat(i+j*2*np)
  end do
end do


end subroutine dirac_diagonal

double precision function ws(r)
implicit none
integer, parameter:: dp=kind(0.d0)
real(dp):: V0,a0,R0,r

V0=1.
a0=1.
R0=1.

ws=-V0/(1+exp((r-R0)/a0))
return
end function ws


!COEFFICIENTI ANGOLARI-SPIN

subroutine cal_F(kf,ki,l,mf,mi,M,F)
integer, parameter:: dp=kind(0.d0)
integer:: kf,ki,l,mf,mi,M
real(dp):: F

if (kf>0) then

  if (ki>0) then
    call scals(l,M,ki,ki-0.5,mi,kf,kf-0.5,mf,F)
  else if (ki<0) then
    call scals(l,M,-ki-1,-ki-0.5,mi,kf,kf-0.5,mf,F)
  end if

else if (kf<0) then

  if (ki>0) then
    call scals(l,M,ki,ki-0.5,mi,-kf-1,-kf-0.5,mf,F)
  else if (ki<0) then
    call scals(l,M,-ki-1,-ki-0.5,mi,-kf-1,-kf-0.5,mf,F)
  end if

end if
end subroutine cal_F

subroutine cal_A(kf,ki,l,mf,mi,M,A)
integer, parameter:: dp=kind(0.d0)
integer:: kf,ki,l,mf,mi,M
real(dp):: A(3)

if (kf>0) then

  if (ki>0) then
    call scalv(l,M,ki,ki-0.5,mi,kf,kf-0.5,mf,A)
  else if (ki<0) then
    call scalv(l,M,-ki-1,-ki-0.5,mi,kf,kf-0.5,mf,A)
  end if

else if (kf<0) then

  if (ki>0) then
    call scalv(l,M,ki,ki-0.5,mi,-kf-1,-kf-0.5,mf,A)
  else if (ki<0) then
    call scalv(l,M,-ki-1,-ki-0.5,mi,-kf-1,-kf-0.5,mf,A)
  end if

end if
end subroutine cal_A

subroutine scalv(l,m,l1,j1,m1,l2,j2,m2,sc)
!f2py intent(in) l,m,l1,j1,m1,l2,j2,m2
!f2py intent(out) sc
     implicit none
     integer ll
     double precision, parameter :: pi=3.14159265358979323848d0
     double precision l,m,l1,j1,m1,l2,j2,m2,sc(3),c,cof3j
     sc=0
     if (l<nint(abs(j1-j2)-1).or.l>nint(j1+j2+1)) return
     ll=mod(nint(l1+l2-1.5+m+m2),2)
     c=(-1)**ll*sqrt((2*j1+1)*(2*j2+1)*(2*l+1)*(2*l1+1)*(2*l2+1)/(4*pi))*cof3j(l,l1,l2,0.d0,0.d0,0.d0)
     if (nint(m-m2+m1).eq.1) then
       sc(1)=-sqrt(2.d0)*c*cof3j(l1,0.5d0,j1,m1-0.5d0,0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2+0.5d0,-0.5d0,-m2) &
    *cof3j(l,l1,l2,-m,0.5d0-m1,m2+0.5d0)
     endif
     if (nint(m-m2+m1).eq.0) then
       sc(2)=-c*(cof3j(l1,0.5d0,j1,m1+0.5d0,-0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2+0.5d0,-0.5d0,-m2) &
      *cof3j(l,l1,l2,-m,-0.5d0-m1,m2+0.5d0) &
     +cof3j(l1,0.5d0,j1,m1-0.5d0,0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2-0.5d0,0.5d0,-m2)*cof3j(l,l1,l2,-m,0.5d0-m1,m2-0.5d0))
     endif
     if (nint(m-m2+m1).eq.-1) then
       sc(3)=sqrt(2.d0)*c*cof3j(l1,0.5d0,j1,m1+0.5d0,-0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2-0.5d0,0.5d0,-m2) &
        *cof3j(l,l1,l2,-m,-0.5d0-m1,m2-0.5d0)
     endif
  end subroutine
  
  
  
  subroutine scals(l,m,l1,j1,m1,l2,j2,m2,sc)
!f2py intent(in) l,m,l1,j1,m1,l2,j2,m2
!f2py intent(out) sc
     implicit none
     integer ll
     double precision, parameter :: pi=3.14159265358979323848d0
     double precision l,m,l1,j1,m1,l2,j2,m2,sc,cof3j
     sc=0
     if (abs(m-m2+m1)> 1.e-12.or.l<abs(j1-j2).or.l>j1+j2) return
     ll=mod(nint(l1+l2-1.5+m+m2),2)
     sc=(-1)**ll*sqrt((2*j1+1)*(2*j2+1)*(2*l+1)*(2*l1+1)*(2*l2+1)/(4*pi))*cof3j(l,l1,l2,0.d0,0.d0,0.d0) &
     *(cof3j(l1,0.5d0,j1,m1+0.5d0,-0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2+0.5d0,-0.5d0,-m2)*cof3j(l,l1,l2,-m,-m1-0.5d0,m2+0.5d0) &
     -cof3j(l1,0.5d0,j1,m1-0.5d0,0.5d0,-m1)*cof3j(l2,0.5d0,j2,m2-0.5d0,0.5d0,-m2)*cof3j(l,l1,l2,-m,-m1+0.5d0,m2-0.5d0))
  end subroutine
  
  
  double precision function clebsch(j1,m1,j2,m2,J,M)
  double precision j1,m1,j2,m2,J,M,cof3j
  clebsch=(-1)**nint(-j1+j2-M)*sqrt(2*J+1)*cof3j(j1,j2,J,m1,m2,-M)
end function


subroutine cal_F_down(kf,ki,l,mf,mi,M,F)
integer, parameter:: dp=kind(0.d0)
real(dp):: kf,ki,l,mf,mi,M
real(dp):: F

if (kf>0) then

  if (ki>0) then
    call scals(l,M,ki-1,ki-0.5,mi,kf-1,kf-0.5,mf,F)
  else if (ki<0) then
    call scals(l,M,-ki,-ki-0.5,mi,kf-1,kf-0.5,mf,F)
  end if

else if (kf<0) then

  if (ki>0) then
    call scals(l,M,ki-1,ki-0.5,mi,-kf,-kf-0.5,mf,F)
  else if (ki<0) then
    call scals(l,M,-ki,-ki-0.5,mi,-kf,-kf-0.5,mf,F)
  end if

end if
end subroutine cal_F_down

subroutine cal_A_down(kf,ki,l,mf,mi,M,A)
integer, parameter:: dp=kind(0.d0)
real(dp):: kf,ki,l,mf,mi,M
real(dp):: A(3)

if (kf>0) then

  if (ki>0) then
    call scalv(l,M,ki-1,ki-0.5,mi,kf-1,kf-0.5,mf,A)
  else if (ki<0) then
    call scalv(l,M,-ki,-ki-0.5,mi,kf-1,kf-0.5,mf,A)
  end if

else if (kf<0) then

  if (ki>0) then
    call scalv(l,M,ki-1,ki-0.5,mi,-kf,-kf-0.5,mf,A)
  else if (ki<0) then
    call scalv(l,M,-ki,-ki-0.5,mi,-kf,-kf-0.5,mf,A)
  end if

end if

end subroutine cal_A_down

