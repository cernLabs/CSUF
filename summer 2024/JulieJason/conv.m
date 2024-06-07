EMG = load("EMGdat.mat");

fieldnames(EMG)
pdat = EMG.Sessiondata;

% take the first 100000 rows from the cell
sampleCell = pdat(1:500000, :);

% make everything we sampled into a string
sampleCellStr = cellfun(@num2str, sampleCell, 'UniformOutput', false);

% open file
fid = fopen('my_data.csv', 'w');

%Loop through the string matrix and write to the file
for i = 1:size(sampleCellStr, 1)
    for j = 1:size(sampleCellStr, 2)
        if j < size(sampleCellStr, 2)
            fprintf(fid, '%s,', sampleCellStr{i, j});
        else
            fprintf(fid, '%s', sampleCellStr{i, j});
        end
    end
    fprintf(fid, '\n');
end

%Close the file
fclose(fid);