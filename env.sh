# See https://github.com/TinyTapeout/tt-gds-action/blob/main/action.yml#L30
#
export OPENLANE2_TAG=2.2.9
export PDK=sky130A
export PDK_ROOT=${HOME}/git/tt/tt08-mydemo/pdk

# https://github.com/TinyTapeout/tt08-verilog-template/issues/13
export TMPDIR=$PWD/tmp
mkdir -p "${TMPDIR}"
