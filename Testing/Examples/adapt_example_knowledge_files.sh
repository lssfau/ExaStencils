for i in `find ../../Examples/ -type f -name "*.knowledge"`; do
    # extract path, dir and basename
    parent_dir_path=$(builtin cd "$(dirname $i)"; pwd)
    echo $parent_dir_path
    dir_name=$(basename $parent_dir_path)
    base=$(basename $i)
    file_name="$dir_name/$base"

    # copy over
    mkdir -p $dir_name
    cp $i $file_name

    # modify copy
    sed -i '/library_CImg/d' $file_name # disable CImg flag
    sed -i 's#../lib#../../../Examples/lib#g' $file_name # adapt path for import
    sed -i -e :a -e '/^\n*$/{$d;N;ba' -e '}' $file_name # delete empty lines at end
    echo '' >> $file_name
    echo 'testing_enabled = true' >> $file_name # enable testing mode

done