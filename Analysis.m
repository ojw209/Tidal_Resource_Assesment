function Stats = Analysis(Data)
Stats = zeros(70,70,1)
%Index's for different Stats, as follows:
%1: Mean of the Absolute Velocity Field
%2: Range of the Absolute Velocity Field
%3: Std. of the Absolute Velocity Field
%4: Co-Variance between the U-V Velocoty Field. 
%Calculate Stats for Abs Velocity Field.
for A_x = 1:70
    for A_y = 1:70
        Stats(A_x,A_y,1) = mean(squeeze(Data(A_x,A_y,:,1)));
        Stats(A_x,A_y,2) = var(squeeze(Data(A_x,A_y,:,2)));
        Var_Temp = cov(squeeze(Data(A_x,A_y,:,3)),squeeze(Data(A_x,A_y,:,3)));
        Stats(A_x,A_y,3) = Var_Temp(2,2); 
    end
end
end
