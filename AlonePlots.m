
% Timeplots for Alone trials run 1

clc 
clear all

part=0;
for i=[1:2,4:6,8:9,11:14,16:22,24:163]; %participant numbers
subj=num2str(i);
part=part+1;

    if i==8 || i==16 || i==24 || i==30 || i==36 || i==45 || i==47 || i==54 || i==60 || i==65 || i==72 || i==74 || i==80 || i==83 || i==86 || i==90 || i==103 || i==105 || i==114 || i==115 || i==118 || i==123 || i==127 || i==131 || i==138 || i==140 || i==150 || i==156 || i==160;
    MRIpart=1;
    MRI=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/MRI/',subj,'/new_Data_P2_',subj,'.csv');
    Data=csvread(MRI,1,0);
    else
    MRIpart=0;    
    Behavioral=strcat('/Users/Kimmie/Dropbox/Papers/BART/Data/BehData/',subj,'/new_Data_P2_',subj,'.csv');
    Data=csvread(Behavioral,1,0);
    end
    

Balloon=length(Data(:,:))-2;
count=0;
ppA=0;
cpA=0;
pA=0;

    for events=1:Balloon
        count=count+1;
        
        trialnr=(Data(count,7))+1;
        
        if Data(count,9)==0 && Data(count,5)==1 && Data(count,21)==1 %Alone run 1 cash in
            cpA=cpA+1;
            cx(cpA)=trialnr; %trialnummer
            cy(cpA)=Data(count,20); %tokens
            pA=pA+1;
            x(pA)=trialnr;
            y(pA)=Data(count,20);
        elseif Data(count,9)==0 && Data(count,5)==1 && Data(count,22)==1 %Alone run 1 pop
            ppA=ppA+1;
            pA=pA+1;
            px(ppA)=trialnr; %trialnummer
            py(ppA)=Data(count,20); %tokens
            x(pA)=trialnr;
            y(pA)=Data(count,20);
        end
       
    end
    
            %plot
            ccx=cx(1:cpA);
            ccy=cy(1:cpA);
            ppx=px(1:ppA);
            ppy=py(1:ppA);
            
            figure
            plot(ccx,ccy,'gs',ppx,ppy,'rs',x,y,'k-');
            legend('cash','pop')
            axis([1 12 0 64])
            title({'Game play in BERP';['Participant ',subj]})
            xlabel('Trial numbers')
            ylabel('Tokens')
            saveas(gcf,strcat('Alone trials',subj,'.png'))
            close all


    x(:)=[];
    y(:)=[];
    cx(:)=[];
    cy(:)=[];
    px(:)=[];
    py(:)=[];

end