SETS

t        time  /1*8760/
j        columns /1*6/
l        loop /1*100/
i        units /Coal, Wind/
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

$onEcho > howToRead_basic_model.txt
par=imput_tech rng=Technologies!A2:G4 cdim=1 rdim=1
par=d rng=Demand!A2:B8761 rdim=1
par=d_down rng=Down_demand!A2:B8761 rdim=1
par=p_wind_ini rng=Wind_prod!A2:B8761 rdim=1
par=cap_wind rng=Wind_cap!A2:B8761 rdim=1
$offEcho

$call gdxxrw.exe Input_data_version7.xlsx output=Input_basic_model.gdx squeeze=n @howToRead_basic_model.txt
$gdxin Input_basic_model.gdx
$load imput_tech d d_down p_wind_ini cap_wind
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



POSITIVE VARIABLE

p(i,t)                         electricity production
new_cap(i)                     investment in new capacity


FREE VARIABLE

C   cost (objective function)

EQUATIONS

obj              objective function
max_prod_base    setting maximum level of production for non variable techs
max_prod_var     setting maximum level of production for variable techs
*dem              meeting the demand
dem_down         meeting the downscaled demand
;

obj..                                C =e= sum((i,t), (v(i)+e(i)+om_v(i))*p(i,t)) + sum(i, (om_f(i)+inv(i))*new_cap(i)) + sum(i, om_f(i)*max_cap_ini(i));
max_prod_base(i,t)$base(i)..         p(i,t) =l= (max_cap_ini(i)+new_cap(i));
max_prod_var(i,t)$var(i)..           p(i,t) =l= p_var_ini(i,t)+new_cap(i)*cap_var(i,t);
*dem(t)..                             sum(i,p(i,t)) =e= d(t);
dem_down(t)..                        sum(i,p(i,t)) =e= d_down(t);

MODEL basic /all/;
SOLVE basic using LP minimizing C;
Display e;

p.l(i,t)$(not p.l(i,t)) = eps;

$onEcho > howToRead_basic_model_to_Excel_1.txt
var = C.l rng=Basic!B2
squeeze=n var = new_cap.l rng=Basic!B5
EpsOut=0 var = p.l cdim=0 rng=Basic!E1
$offEcho

execute_unload "basic_model_1.gdx" C.l p.l new_cap.l
execute 'gdxxrw.exe basic_model_1.gdx o=basic_model_year.xls @howToRead_basic_model_to_Excel_1.txt'

PARAMETERS
p_dep_e(i,l,t)           how production changes depending on emission cost
C_dep_e(l)               how production changes depending on emission cost
new_cap_dep_e(i,l)       how new capacity changes depending on emission cost
rep_dep_e(i,l)           repartition of wind and coal depending on emission cost
test_e(l)
;

loop(l, e('Coal')=ord(l)-1;
SOLVE basic using LP minimizing C;
p_dep_e(i,l,t)=p.l(i,t);
C_dep_e(l)=C.l;
new_cap_dep_e(i,l)=new_cap.l(i);
test_e(l) = e('Coal')
Display e
);

new_cap_dep_e(i,l)$(not new_cap_dep_e(i,l)) = eps;

loop((i,l),rep_dep_e(i,l)=Sum(t,p_dep_e(i,l,t))/Sum(t,d_down(t)));

$onEcho > howToRead_basic_model_to_Excel_2.txt
par = rep_dep_e cdim=1 rdim=1 rng=Prod_dep_emission!A1
par = C_dep_e rng=Cost_dep_emission!A1
par = test_e rng=Test!A1
EpsOut=0 par = new_cap_dep_e cdim=1 rdim=1 rng=New_cap_dep_emission!A1
$offEcho

execute_unload "basic_model_2.gdx" C_dep_e new_cap_dep_e rep_dep_e test_e
execute 'gdxxrw.exe basic_model_2.gdx o=basic_model_year.xls @howToRead_basic_model_to_Excel_2.txt'



