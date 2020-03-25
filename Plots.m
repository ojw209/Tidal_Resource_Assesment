%File Name Parameters
F_File_All = 'MARC_F1-MARS3D-ARMOR_2018';
F_Month = '01';
F_Day = '01';
F_Time = 'T0100Z';
F_Extens = '.nc';

%Read file infomation.
File_Name = strcat(F_File_All,F_Month,F_Day,F_Time,F_Extens);

%Dimensions of file to read
Read_Start = [240 240];
Read_End = [70 70];

%Function to access infomation about the model data.
ncdisp(File_Name)

% NOTE:
% Arakawa grid type = 'C1', OCO oriented grid.
% Useful Variables:
% ni = x-dimension of the grid
% nj = y-dimension of the grid
% H0 = bathymetry relative to the mean level
% U = barotropic zonal velocity
% V = barotropic meridional velocity
% UZ = 3d zonal velocity
% VZ = 3d meridional velocity

UZ_Data = Data(:,:,1,1)
VZ_Data = Data(:,:,1,1)
%Read in Data.
%[Velocity Field - NOTE: Query Difference between U/V and UZ/VZ]
%UZ_Data = ncread(File_Name,'UZ',[Read_Start 1 1],[Read_End 1 inf]);
%VZ_Data = ncread(File_Name,'VZ',[Read_Start 1 1],[Read_End 1 inf]);
Bath_Data = ncread(File_Name,'H0',Read_Start,Read_End);
Flow_Data = sqrt((UZ_Data.*UZ_Data)+(VZ_Data.*VZ_Data));
Z_Plane = zeros(70,70);

%Bathymetric Plot
b_plot = figure('Name','Bathymetric Plot');
surf(Bath_Data','EdgeAlpha',0.5);
view(2);
colorbar;

%Mag Flow Plot
Flow_plot = figure('Name','Mag Flow Velocity Plot');
surf(Flow_Data','EdgeAlpha',0.5);
view(2);
colorbar;

%Velocity Plot 
Vel_plot = figure('Name','Velocity Field Alderney Race')
quiver3(Z_Plane,[UZ_Data]',[VZ_Data]',Z_Plane);
view(2);

%Site Specific Plots - [Requires full temporal dataset.]
%Conversion from cartesian to long, lat. Needs to be carried out. 
% 
%Tidal Flow over time. 
Flow_Time = figure('Name','Magnitude of Flow over Time')
X = 30;
Y = 30;
Z = 40;
plot(squeeze(Data(X,Y,Z,:,1)))
% 
%Mean Mag Flow Over Time
Time_Flow_plot = figure('Name','Mean Magnitude of flow over Time');
surf(Stat_Field(:,:,1)','EdgeAlpha',0.5);
view(2);
colorbar;

Surf_inter_temp = zeros(4900,3);
Index_Counter = 0;
for i = 1:70
    for j = 1:70
        Index_Counter = Index_Counter + 1
        Surf_inter_temp(Index_Counter,1) = i;
        Surf_inter_temp(Index_Counter,2) = j;
        Surf_inter_temp(Index_Counter,3) = Flow_Data(i,j);
    end
end

Surf_inter_temp(isnan(Surf_inter_temp)) = 0;
%Pre-interpolation
plot3(Surf_inter_temp(:,1),Surf_inter_temp(:,2),Surf_inter_temp(:,3))

%Need to consider the effects of the coastline boundary conditions on the
%interopolation and how to mitigate these effects.[Explore interpolation
%with Spines]
Func_Surf = fit([Surf_inter_temp(:,1),Surf_inter_temp(:,2)],Surf_inter_temp(:,3),'linearinterp')