
% In data file: 0=Alone, 1=LP (p2=22222 is human, <10=CSP), 2=SB, 2+P2=2 = SIM, 
% Condition: 0 = Alone, 1 = SB_human, 2 = SIM, 3=LP, 4=LP_comp
%CSP 2=NONSOC, 3=HES, 4=AGGR

% Generate 6 lines per subject, indicating:
% - Participant number;
% - whether participant was MRI or behavioral participant;
% - mean cash in during BERP Alone trials;
% - Mean belief regarding balloon pops;
% - MaxBalloonSize
% - First cash in of BERP Alone trials

clc 
clear all

cash=1;
k=0;

% Establish column names in data file
MRIBART{cash,1}='Participant';
MRIBART{cash,2}='MRI'; %0=no, 1=yes
MRIBART{cash,3}='Alone';
MRIBART{cash,4}='BeliefBalloon';
MRIBART{cash,5}='MaxBalloonSize';
MRIBART{cash,6}='firstcash';
MRIBART{cash,7}='popsbeforefirstcash';

% Loop through all participants' folders and extract data
for i=[1:2,4:6,8:9,11:14,16:22,24:163];
subj=num2str(i);
k=k+1;

% Data stemming from MRI participant folders
if i==8 || i==16 || i==24 || i==30 || i==36 || i==45 || i==47 || i==54 || i==60 || i==65 || i==72 || i==74 || i==80 || i==83 || i==86 || i==90 || i==103 || i==105 || i==114 || i==115 || i==118 || i==123 || i==127 || i==131 || i==138 || i==140 || i==150 || i==156 || i==160;
MRIpart=1;
MRI=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/MRI/',subj,'/new_Data_P2_',subj,'.csv'); %game play

MaxBalloonFile=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/MRI/MaxRatings.csv'); %Max balloon rating
MaxBalData=csvread(MaxBalloonFile,1,0);

    for y=1:length(MaxBalData)
        if MaxBalData(y,2)==i
        MaxBalloon=MaxBalData(y,3);    
        end
    end

Belief=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/MRI/',subj,'/DistRatings_',subj,'.csv'); %Beliefs balloon pops

% Load the data
Data=csvread(MRI,1,0);
BeliefData=csvread(Belief,0,0);

else
% Data stemming from behavioral participants
MRIpart=0;    
Behavioral=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/BehData/',subj,'/new_Data_P2_',subj,'.csv'); %game play
Belief=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/BehData/',subj,'/DistRatings_',subj,'.csv'); %Beliefs balloon pops

MaxBalloonFile=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/BehData/MaxRatings.csv'); %Max balloon rating
MaxBalData=csvread(MaxBalloonFile,1,0);

    for y=1:length(MaxBalData)
        if MaxBalData(y,2)==i
        MaxBalloon=MaxBalData(y,3);    
        end
    end
    
    % Load the data
    Data=csvread(Behavioral,1,0);
    BeliefData=csvread(Belief,0,0);
    
end

SumBeliefBalloon=sum(BeliefData(2,:));

%Beliefs --> don't divide by 100 but sum of all numbers used by participants in drawing distribution.
BeliefBalloon=[BeliefData(2,1)/SumBeliefBalloon;BeliefData(2,2)/SumBeliefBalloon;BeliefData(2,3)/SumBeliefBalloon;BeliefData(2,4)/SumBeliefBalloon;...
    BeliefData(2,5)/SumBeliefBalloon;BeliefData(2,6)/SumBeliefBalloon;BeliefData(2,7)/SumBeliefBalloon;BeliefData(2,8)/SumBeliefBalloon;...
    BeliefData(2,9)/SumBeliefBalloon;BeliefData(2,10)/SumBeliefBalloon;BeliefData(2,11)/SumBeliefBalloon;BeliefData(2,12)/SumBeliefBalloon;...
    BeliefData(2,13)/SumBeliefBalloon;BeliefData(2,14)/SumBeliefBalloon;BeliefData(2,15)/SumBeliefBalloon;BeliefData(2,16)/SumBeliefBalloon;...
    BeliefData(2,17)/SumBeliefBalloon;BeliefData(2,18)/SumBeliefBalloon;BeliefData(2,19)/SumBeliefBalloon;BeliefData(2,20)/SumBeliefBalloon];
BeliefBalloonsize=[(0.05*MaxBalloon),(0.1*MaxBalloon),(0.15*MaxBalloon),(0.2*MaxBalloon),(0.25*MaxBalloon),(0.3*MaxBalloon),(0.35*MaxBalloon),...
    (0.4*MaxBalloon),(0.45*MaxBalloon),(0.5*MaxBalloon),(0.55*MaxBalloon),(0.6*MaxBalloon),(0.65*MaxBalloon),(0.7*MaxBalloon),...
    (0.75*MaxBalloon),(0.8*MaxBalloon),(0.85*MaxBalloon),(0.9*MaxBalloon),(0.95*MaxBalloon),(1*MaxBalloon)];
MeanBeliefBalloon=BeliefBalloonsize*BeliefBalloon;

Fulldis((k*20-19):(k*20),1)=BeliefBalloon;
Fulldis((k*20-19):(k*20),2)=1:20;
Fulldis((k*20-19):(k*20),3)=i;

% Counters
Balloon=length(Data(:,4));
count=0;
AC=0;

% cashed in tokens
    for events=1:Balloon       
        count=count+1;
        
        if Data(count,9)==0 && Data(count,21)==1 %Alone condition cashed-in events
        AC=AC+1;    
        condition=0;
        Alone(AC,i)=Data(count,20); %tokens when cashed in
        Trial(AC,i)=Data(count,6); %on which trial cashed in
        end
            
    end
    
    MeanAlone=(sum(Alone(:,i)))/AC; %mean cashed in tokens for Alone. Will be stored in sep column for each new subj.
    FirstAlone=Alone(1,i); %first cashed in token
    FirstTrialCash=Trial(1,i); %first trial when cashed in.
 
    cash=cash+1;
    MRIBART{cash,1}=Data(count,3); %Participant number
    MRIBART{cash,2}=MRIpart; %MRI or behavioral participant
    MRIBART{cash,3}=MeanAlone; % Mean BERP Alone 
    MRIBART{cash,4}=MeanBeliefBalloon; %Mean beliefs balloon pop point
    MRIBART{cash,5}=MaxBalloon; %MaxBalloon;
    MRIBART{cash,6}=FirstAlone; %MaxBalloon;
    MRIBART{cash,7}=FirstTrialCash; %MaxBalloon;
     
end

%Save in csv file
fid = fopen('FirstTrial.csv', 'w') ;
 fprintf(fid, '%s,', MRIBART{1,1:end-1}) ;
 fprintf(fid, '%s\n', MRIBART{1,end}) ;
 fclose(fid) ;
 dlmwrite('FirstTrial.csv', MRIBART(2:end,:), '-append') ;
 
%  % Save distribution in csv file
%   Write=Fulldis;
%   dlmwrite(strcat('BBdis.txt'), Write);
 
