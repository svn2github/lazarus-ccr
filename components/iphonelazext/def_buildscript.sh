## The script builds the project for both 32 and 64-bit architectures
## It's requirement for Apple to support both arm32 and arm64 for a device
## Some Simulators might still be 32-bit (iPhone4, iPads), thus both x86 and x86_64 
## are compiled  
echo "compiling FPC project"

export RESULT_EXE=${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}
cd $FPC_MAIN_DIR

export TargetCPU=${PLATFORM_PREFERRED_ARCH}
## default compile options, assuming ARM
export OPT32=${FPC_OPT_A32}
export OPT64=${FPC_OPT_A64}
export CPU32="arm"
export CPU64="aarch64"
export TargetOS="darwin"

## Simulator has been selected in Xcode project
## switching options to Intel 
if [ "${PLATFORM_NAME}" == "iphonesimulator" ]; then
export OPT32=${FPC_OPT_I32}
export OPT64=${FPC_OPT_I64}
export CPU32="i386"
export CPU64="x86_64"
export TargetOS="iphonesim"
fi

## 64-bit compilation

export Result64=${RESULT_EXE}_64
export TargetCPU=${CPU64}
export Target=${TargetCPU}-${TargetOS}

## making output directory
export outdir=lib/${Target}
mkdir -p ${outdir}

## debugging echo 
echo ${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} \
 ${FPC_CUSTOM_OPTIONS} ${OPT64} \
 -Filib/${Target} -FUlib/${Target} \
 -XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -o${Result64}

${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} \
 ${FPC_CUSTOM_OPTIONS} ${OPT64} \
 -Filib/${Target} -FUlib/${Target} \
 -XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -o${Result64}

export RES=$?

## if compiler returned an error, stop the script and exit with error
if [ $RES != 0 ]; then
  exit $RES
fi


## 32-bit complication

export Result32=${RESULT_EXE}_32
export TargetCPU=${CPU32}
export Target=${TargetCPU}-${TargetOS}
## making output directory
export outdir=lib/${Target}
mkdir -p ${outdir}

echo ${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} \
 ${FPC_CUSTOM_OPTIONS} ${OPT32} \
 -Filib/${Target} -FUlib/${Target} \
 -XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -o${Result32}

${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} \
 ${FPC_CUSTOM_OPTIONS} ${OPT32} \
 -Filib/${Target} -FUlib/${Target} \
 -XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -o${Result32}
export RES=$?

## if compiler returned an error, stop the script and exit with error
if [ $RES != 0 ]; then
exit $RES
fi

## both 32 and 64 are done, making the fat binary
lipo -create ${Result32} ${Result64} -output ${RESULT_EXE}

## removing 32 and 64 bit
rm ${Result32}
rm ${Result64}

exit $FPCRES
