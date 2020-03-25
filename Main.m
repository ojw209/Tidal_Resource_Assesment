%File Name Parameters
F_File_All = 'MARC_F1-MARS3D-ARMOR_2018';
F_Month = '01';
F_Day = '02';
F_Time = 'T0800Z';
F_Extens = '.nc';

%Data_Store Variables
Data = zeros(70,70,1238,4);

%Area to Read
Read_Start = [240 240];
Read_End = [70 70];

C_Day = 0;
C_Time= 0;
%Days in each month. Non-Leap Year.
D_In_Month = [31 28 31 30 31 30 31 31 30 31 30 31];
%Iterate Across All Days
for Month = 1:1:12
    if Month < 10
        F_Month = strcat('0',int2str(Month));
    end
    
    if Month >= 10
        F_Month = int2str(Month);
    end
    
    for Day = 1:1:D_In_Month(Month)
        C_Day = C_Day + 1;
        if Day < 10
            F_Day = strcat('0',int2str(Day));
        end
        
        if Day >= 10
            F_Day = int2str(Day);
        end
        
        for Time = 0:1:23
            C_Time = C_Time + 1;
            if Time < 10
                
                F_Time = strcat('T0',int2str(Time),'00Z');
            end
            
            if Time >= 10
                
                F_Time = strcat('T',int2str(Time),'00Z');
            end
            disp([F_Time F_Day F_Month])
            File_Name = strcat(F_File_All,F_Month,F_Day,F_Time,F_Extens);
            %Interesting Stuff 
            STORE_UZ_Data(:,:,C_Time) = ncread(File_Name,'UZ',[Read_Start 40 1],[Read_End 1 1]); 
            STORE_VZ_Data(:,:,C_Time) = ncread(File_Name,'VZ',[Read_Start 40 1],[Read_End 1 1]);
        end
    end
end
Data(:,:,:,1) = sqrt((STORE_UZ_Data.*STORE_UZ_Data)+(STORE_VZ_Data.*STORE_VZ_Data));
Data(:,:,:,2) = STORE_UZ_Data;
Data(:,:,:,3) = STORE_VZ_Data;

Stat_Field = Analysis(Data);
