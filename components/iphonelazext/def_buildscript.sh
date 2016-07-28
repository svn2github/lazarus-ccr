## start
echo "compiling FPC project"

export RESULT_EXE=${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}
export IOSHEADERS=
cd $FPC_MAIN_DIR
#rm $RESULT_EXE
export TargetCPU=${PLATFORM_PREFERRED_ARCH}

if [ "${PLATFORM_NAME}" == "iphonesimulator" ]; then
  export TargetOS="iphonesim"
fi
export Target=${TargetCPU}-${TargetOS}

pwd
echo ${RESULT_EXE}

${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} -MDelphi -Scghi -O1 -l -dIPHONEALL \
 ${FPC_CUSTOM_OPTIONS} \
-Filib/${Target} -FUlib/${Target} \
-XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -o${RESULT_EXE}
export RES=$?

if [ $RES != 0 ]; then
  exit $RES
fi

echo ${RESULT_EXE}

exit $FPCRES