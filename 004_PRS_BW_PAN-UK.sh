
Rscript /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/scripts/Templates_PRSice_v2.0/PRSice.R --dir . \
--prsice /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/scripts/Templates_PRSice_v2.0/PRSice_linux \
--base /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/base_data/002_fBW/PanUK/base_data.txt \
--target /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/gwas/Final_data_HRCimp_QC2/HELIX.impQC.rs.EUR.nosexchr \
--extract /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR_panUK.valid \
--thread 16 \
--beta \
--stat BETA \
--A1 A1 \
--A2 A2 \
--pvalue P \
--clump-r2 0.1 \
--bar-levels 0.00000005,0.000005,0.0001,0.001,0.01,0.05,0.1,0.2,0.5,1 \
--binary-target F \
--pheno /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/repr/pheno_data_eur_zbw21.tsv \
--pheno-col zBW \
--cov /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/repr/pheno_data_eur_zbw21.tsv \
--cov-col @PC[1-10] \
--fastscore T \
--quantile 100 \
--quant-break 1,5,10,20,40,60,80,90,95,99,100 \
--quant-ref 60 \
--chr-id c:L \
--chr CHR \
--bp BP \
--perm 10000 \
--all-score \
--out /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR_panUK 

