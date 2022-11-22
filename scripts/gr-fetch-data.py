import pandas as pd
from neuromaps.datasets import fetch_annotation
from nilearn import datasets, image, maskers

# define source map and atlas parcellation
src = image.load_img("siips.nii.gz")
atlas = datasets.fetch_atlas_basc_multiscale_2015(version="asym")["scale444"]

# extract prediction weights from src regions defined by atlas
region_masker = maskers.NiftiLabelsMasker(atlas)
src_region_mask = region_masker.fit_transform(src)[0]

# initialize data frame with response variable
df = {"siips": src_region_mask}

# iterate through all neuromaps MNI152 annotations
# skipping "beliveau2017" bc of sparse spatial coverage

for key, img in fetch_annotation(space="MNI152").items():

    if key[0] == "beliveau2017":
        continue

    # resample voxel dimensions of predictors to match response
    img_resamp = image.resample_to_img(img, src, interpolation="continuous")

    # extract regional averages from each annotation
    region_masked = region_masker.fit_transform(img_resamp)[0]

    # add regional averages to data frame
    df[f"{key[0]}-{key[1]}"] = region_masked

# save data frame to csv file
pd.DataFrame(df).to_csv("../data/neuromaps-mni152_y-siips_res-basc444.csv", index=False)
