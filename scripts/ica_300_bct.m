%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Carlos Rodriguez, PhD. MRN
% 05/17/2021
% modified 10/5/2021
% ica_300_bct.m
% This script will generate the graph metrics for all subjects and all runs
% with the specified components. 
%
% Usage:
% Run script as file paths have been pre-specified
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Fix Matlab 2020b
rehash toolboxcache
restoredefaultpath

% Set paths for the toolboxes
addpath(genpath('/export/research/analysis/human/jhouck/cobre06_65007/carlos_work/tools/gift-master'))
addpath(genpath('/export/research/analysis/human/jhouck/cobre06_65007/carlos_work/tools/BCT'))

% Load data
load('/export/research/analysis/human/jhouck/abcd/ica_300/ica_output/ica_300_sdrtwa/sdrtwa__postprocess_results.mat')

% Select components to analyze
comps = [4, 9, 13, 21, 28, 29, 30, 32, 33, 35, 36, 39, 45, 47, 51, 53, 55, 57, 59, 61, 62, 63, 65, 67, 71, 73, 77, 78, 79, 81, 84, 85, 90, 94, 96, 97]

% Load the subjectkeys / NDAR GUIDs
load('/export/research/analysis/human/jhouck/abcd/ica_300/ica_output/ica_300_sdrtwa/sdrtwa_subjectkeys.mat')

% Initialize and empty table
T = table;

for r = 1:4 % 1 through the 4 runs collected
    
    % Specify which run in the ICA
    run = ['run-' num2str(r)];
    
    % Get the fnc matrix for all subjects in specified run
    fnc = squeeze(fnc_corrs_all(:, r, :, :));
    
    % Select the components
    fnc = fnc(:, comps, comps);
    
    % Convert to edge weights from z-scores
    fnc = tanh(fnc);
    
    % Get absolute values, could also try positive values only
    fnc = abs(fnc);
    
    % Specify the number of subjects
    nSub = size(fnc,1); % number of subjects
    
    % Sets proportion thresholds, from .1 to .5 by increments of .1
    k = (.1:.1:.5); %
    
    % Set labels
    labels = {'modularity', 'chpath', 'eff_glob', 'ave_clust_coeff'};
    
    % Initialize empty structures
    data.modularity = zeros(nSub,size(k,2)); %modularity
    data.chpath = zeros(nSub, size(k,2)); %characteristic path length
    data.eff_glob = zeros(nSub, size(k,2)); %global efficiency
    data.ave_clust_coeff = zeros(nSub, size(k,2)); %average clustering coefficient

    % Calculate graph theory metrics for each subject
    t = table;

    for sub = 1:nSub
        
        % Remove singleton dimensions
        a = squeeze(fnc(sub,:,:));
        
        % Loop to go through several graph metric thresholds
        for j = 1:size(k,2) % loops through 5 levels of k      
            
            m = threshold_proportional(a, k(j)); % Determines the level of k

            % modularity
                [Ci, Q] = modularity_und(m);
                data.modularity(sub, j) = Q;
                
            % average clustering coefficient, weights need to be btw 0 and 1
                data.ave_clust_coeff(sub, j) = mean(clustering_coef_wu(m));
                
            % transitivity, weights need to be btw 0 and 1
                %data.transt(sub, j) = transitivity_wu(m);
                
            % global efficiency
                data.eff_glob(sub, j) = efficiency_wei(m);
                
            % characteristic path length. Requires a distance matrix
                d = distance_wei_floyd(m,'inv'); %or use weight_converstion to get a length matrix
                data.chpath(sub, j) = charpath(d, 0, 0); % set 0, 0 to ignore diagonals and infs             
        end
        
        % Repmat is set to 5 because there are 5 thresholds for each
        % measure, and set up to make it a long data set
        tt = table(repmat(subject_keys(sub), 5, 1), repmat(run, 5, 1), k', data.modularity(sub,:)', data.ave_clust_coeff(sub,:)', data.chpath(sub,:)', data.eff_glob(sub,:)');
        t = [t; tt];
    end
    % append to table
    T = [T; t];
end

% Change column names
T.Properties.VariableNames = {'subject', 'run', 'k', 'mod', 'clust', 'cpath', 'geff'};

% Write table out to .csv file in long format
cd('/export/research/analysis/human/jhouck/cobre06_65007/carlos_work/ica_300/ica_300_bct/data')
writetable(T, '/export/research/analysis/human/jhouck/cobre06_65007/carlos_work/ica_300/ica_300_bct/data/ica_300_graph_metrics.csv') 



  
