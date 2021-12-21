for i in `find ../../Examples/ -type f -name "*.knowledge"`; do
    # extract path, dir and basename
    parent_dir_path=$(builtin cd "$(dirname $i)"; pwd)
    echo $parent_dir_path
    dir_name=$(basename $parent_dir_path)
    base=$(basename $i)
    file_name="$dir_name/$base"

    # copy
    mkdir -p $dir_name
    cp $i $file_name

    # modify copy
    sed -i '/library_CImg/d' $file_name
    echo '' >> $file_name
    echo 'testing_enabled = true' >> $file_name
    sed -i 's#../lib#../../../Examples/lib#g' $file_name

done