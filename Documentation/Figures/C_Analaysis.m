% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Soot spectral radiation                                    %
% To analyze different mean values of c for Reyleight regime %
% Plankc weighted of two different parameters  C and C*eta   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coded by Hadi Bordbar, Aalto University, May 2020          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc;clear;
close all

% needed constants
SIGMA=5.67036713e-8;
c0=2.998e8; % speed of light in vaccum (m/s)
h=6.626e-34; %  Planck constant (J s)
k=1.3807e-23; %   Boltzmann constant(J/K)

% case settings
deta=0.02; % spectral resolution cm-1
T=[300:50:3000]; % Temperature range of interest (K)
eta=[50:deta:10000];% wavelength
mu=10000./eta; % wavelnegth micro-m

% Change and Charalampopoulos correlations for soot complex index of
% refraction
n_mu = 1.811 + 0.1263.*log(mu) + 0.0270.*(log(mu)).^2 + 0.0417.*(log(mu)).^3; 
k_mu = 0.5821 + 0.1213.*log(mu) + 0.2309.*(log(mu)).^2 + 0.0100.*(log(mu)).^3;
C0=36*pi.*n_mu.*k_mu./((n_mu.^2-k_mu.^2+2).^2+4.*n_mu.^2.*k_mu.^2);

for iT=1:length(T)
  Ip=((2*h*c0^2)*((eta*100).^3))./(exp((h*c0*eta*100)./(k*T(iT)))-1);%w/(m2 sr m-1) % Planck Intensity
  IP_C=Ip.*C0*deta*100;
  IP_C_ETA=Ip.*C0.*eta*100*deta*100;
  IP=Ip*deta*100;
  IP_eta=Ip.*eta*100*deta*100;
  C_P(iT)=sum(IP_C)/sum(IP);
  C_P_eta(iT)=sum(IP_C_ETA)/sum(IP_eta);
end

% plotting
figure(1); plot(T, C_P,'k-*');
xlabel('T(K)'); 
ylabel('$\bar{C_P}$','Interpreter','Latex');
paperunits='centimeters';
filewidth=12;%cm
fileheight=8;%cm
size=[filewidth fileheight];
set(gcf,'paperunits',paperunits,'paperposition',[0 0 size]);
set(gcf, 'PaperSize', size);

H=figure(1);
filename='Fig1_P_C';
% savefig(H, filename);
print(H,'-dpdf',filename);


figure(2); plot(T, C_P_eta,'k-*');
xlabel('T(K)'); 
ylabel('$\bar{C_{P,\omega}}$','Interpreter','Latex')
paperunits='centimeters';
filewidth=12;%cm
fileheight=8;%cm
size=[filewidth fileheight];
set(gcf,'paperunits',paperunits,'paperposition',[0 0 size]);
set(gcf, 'PaperSize', size);
H=figure(2);
filename='Fig2_P_C_eta';
% savefig(H, filename);
print(H,'-dpdf',filename);
    
        


