# NWT
This repo is dedicated to the NWT integrate project from ECCC/BAM. As it has proprietary data, it is a private repo.

Please, use this repo to upload all code related to the project here:

* **Shared data**: The `inputs` folder is the place for (small and specific) shared data. Please note that your code should, preferably, be able to download data from fixed repositories (i.e. the project's GDrive folder) as github has a limit on the size of files that it can upload. This is why most of the contents of this folder are in the `.gitignore` file. If you add data to your local `inputs` folder that you want to be uploaded to github, please specify the file's name in the `.gitignore` file with the format `!dataToUpload.ext`.

* **Shared outputs**: For the same reason as above, the folder `outputs` is also in `.gitignore`.

* **SpaDES modules**: Should be inside the folder `modules`. Remember that to create a SpaDES module, you can easily install and update the `SpaDES` library and use the command: newModule("moduleName", path/to/folder).

* **Random or temporary codes**: Place these codes in the `temp` folder. Please, keep in mind that once the project is over, we should be able to remove these files without a problem.

* **Shared functions** : The folder `functions` should host the ones that are used outside of modules.

* Diana created her own repo for the national bird model (`NationalModel` folder), so I added a submodule of it in this repo. This is like creating a shortcut in Windows for a specific file.

Please, feel free to edit this file if you feel like. :)
