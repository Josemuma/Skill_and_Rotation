% Need to be: (E)quity-(D)omestic-style(Y) Growth,growth&income(B)
% EDYG EDYB 

% From running mfLink.m
size(unique(mfLink2.crsp_fundno))
size(unique(mfLink2.wficn))
mfLink2 = removevars(mfLink2, {'fdate'});
mfLink2Unique = unique(mfLink2(:,2));
% If we join with ´morethan60´there are 6,866

% Second filter running crspImport.m
% Intersect crsp_fundno and mfLinkUnique = 3,678
crspnAndMflink = innerjoin(mfLink2Unique,crsp_fundno,'LeftKeys',{'crsp_fundno'},...
    'RightKeys',{'Var1'});

% Then intersect with ´morethanX´ = 3,150
Funds2Use = innerjoin(crspnAndMflink,morethanX,'LeftKeys',{'crsp_fundno'},...
    'RightKeys',{'morethanX'});

% With  returns
returns = innerjoin(Funds2Use,returnsNew,'Keys',{'crsp_fundno'});

% Save to use in R
writetable(returns, "FundsAndReturns");
writetable(FourFactors, "FourFactors");
writetable(returnsNew, "returnsNew");
