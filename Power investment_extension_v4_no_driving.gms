SETS

t        time  /1*8760/
j        columns /1*6/
i        units /Coal, Wind/
k        cars/1*20/
base(i)
var(i)
;

base(i)$(ord(i)=1)=yes;
var(i)$(ord(i)=2)=yes;
;

PARAMETERS

imput_tech(i,j)     imput data for technologies characteristics
d(t)                demand in a representative month
d_down(t)           downscaled demand
p_wind_ini(t)       production of wind turbines in a representative month (before investment)
cap_wind(t)         capacity factor of wind at each time
dcars(t,k)          demand for driving
rep(i)

car_unload_cap      unload car capacity  /0.270/
car_load_cap        load car capacity    /0.270/
max_car_storage     maximum car storage capacity /0.0934/
n_cars(k)           number of cars /1 28,2 66,3 68,4 143,5 160,6 174,7 188,8 213,9 227,10 242,11 244,12 256,13 303,14 360,15 368,16 428,17 471,18 472,19 526,20 1102/

$onEcho > howToRead_extension_model.txt
par=imput_tech rng=Technologies!A2:G4 cdim=1 rdim=1
par=d rng=Demand!A2:B9761 rdim=1
par=d_down rng=Down_demand!A2:B9761 rdim=1
par=p_wind_ini rng=Wind_prod!A2:B9761 rdim=1
par=cap_wind rng=Wind_cap!A2:B9761 rdim=1
par=dcars rng=Cars_demand!A2:U8762 rdim=1 cdim=1
$offEcho

$call gdxxrw.exe Input_data_version7.xlsx output=Input_extension_model.gdx squeeze=n @howToRead_extension_model.txt
$gdxin Input_extension_model.gdx
$load imput_tech d d_down p_wind_ini cap_wind dcars
$gdxin


v(i)             variable cost
om_f(i)          fixed O&M cost
om_v(i)          variable O&M cost
inv(i)           investment cost
max_cap_ini(i)   maximum initial capacity
e(i)             emission cost
p_var_ini(i,t)   production of variable techs in a representative month (before investment)
cap_var(i,t)     capacity factor of variable techs
;

loop(i,v(i)=imput_tech(i,'1'));
loop(i,om_f(i)=imput_tech(i,'2'));
loop(i,om_v(i)=imput_tech(i,'3'));
loop(i,inv(i)=imput_tech(i,'4'));
loop(i,max_cap_ini(i)=imput_tech(i,'5'));
loop(i,e(i)=imput_tech(i,'6'));


loop(t,p_var_ini('Wind',t)=p_wind_ini(t));
loop(t,cap_var('Wind',t)=cap_wind(t));

*INGRID ADD
loop((t,k),dcars(t,k)=0);



POSITIVE VARIABLE

p(i,t)                         electricity production
new_cap(i)                     investment in new capacity
storageload(t,k)
storageunload(t,k)
storagelevel(t,k)

FREE VARIABLE

C   cost (objective function)

EQUATIONS

obj              objective function
max_prod_base    setting maximum level of production for non variable techs
max_prod_var     setting maximum level of production for variable techs
*dem              meeting the demand
dem_down         meeting the downscaled demand

*TOMAS ADD
demandcar        meeting the cars demand
storagelevelc    meeting the storage equallity
max_unload
max_load
max_storage
unload_from_battery
;

obj..                                C =e= sum((i,t), (v(i)+e(i)+om_v(i))*p(i,t)) + sum(i, (om_f(i)+inv(i))*new_cap(i)) + sum(i, om_f(i)*max_cap_ini(i));
max_prod_base(i,t)$base(i)..         p(i,t) =l= (max_cap_ini(i)+new_cap(i));
max_prod_var(i,t)$var(i)..           p(i,t) =l= p_var_ini(i,t)+new_cap(i)*cap_var(i,t);
**Tomas modified
*dem(t)..                             sum(i,p(i,t))+sum(k,(storageunload(t,k)-storageload(t,k))*n_cars(k)) =e= d(t)+sum(k,dcars(t,k)*n_cars(k));
dem_down(t)..                        sum(i,p(i,t))+sum(k,(storageunload(t,k)-storageload(t,k))*n_cars(k)) =e= d_down(t)+sum(k,dcars(t,k)*n_cars(k));
demandcar(k,t)..                     storagelevel(t,k)*n_cars(k) =g= dcars(t,k)*n_cars(k);
**TOMAS ADD
unload_from_battery(k,t)..           storageunload(t,k) =g= dcars(t,k);
max_unload(k,t)..                    storageunload(t,k) =l= car_unload_cap;
max_load(k,t)..                      storageload(t,k) =l= car_load_cap;
max_storage(k,t)..                   storagelevel(t,k) =l= max_car_storage;
storagelevelc(k,t)..                 storagelevel(t+1,k) =e= storagelevel(t,k)+storageload(t,k)-storageunload(t,k);



MODEL ED /all/;
SOLVE ED using LP minimizing C;


p.l(i,t)$(not p.l(i,t)) = eps;

rep(i)=Sum(t,p.l(i,t))/(Sum(t,d_down(t))+ Sum((t,k),dcars(t,k)*n_cars(k)-(storageunload.l(t,k)-storageload.l(t,k))*n_cars(k)));

$onEcho > howToRead_extension_model_to_Excel.txt
var = C.l rng=No_driving!B2
par = rep rng=No_driving!B8
squeeze=n var = new_cap.l rng=No_driving!B5
EpsOut=0 var = p.l cdim=0 rng=No_driving!E1
$offEcho

execute_unload "extension_model.gdx" C.l p.l new_cap.l rep
execute 'gdxxrw.exe extension_model.gdx o=extension_model.xls @howToRead_extension_model_to_Excel.txt'

