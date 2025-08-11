# SPDX-FileCopyrightText: © 2024 Tiny Tapeout
# SPDX-License-Identifier: Apache-2.0

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles, Edge, FallingEdge, First, RisingEdge


def time(unit) -> int:
    return cocotb.utils.get_sim_time(unit)


def time_us():
    return time("us")


def time_ms():
    return time("ms")


def time_clocks(clock):
    return time("step") / clock.period


async def wait_cycles(dut, clock, n, trigger):
    t0 = time_clocks(clock)
    # await First(ClockCycles(clock.signal, n), trigger)
    await trigger
    t1 = time_clocks(clock)
    delta = t1 - t0
    dut._log.info("trigger signal: %d, after %f clocks", trigger.signal.value, delta)
    assert int(delta) == n


@cocotb.test()
async def test_project(dut):
    dut._log.info("Start")

    # Set the clock period to 25.175 MHz
    clock = Clock(dut.clk, 39.722, units="ns")
    cocotb.start_soon(clock.start())

    # Reset
    dut._log.info("Reset")
    dut.ena.value = 1
    dut.ui_in.value = 0
    dut.uio_in.value = 0
    # dut.uio_in.value = 1
    dut.rst_n.value = 0
    await ClockCycles(dut.clk, 10)
    dut.rst_n.value = 1

    dut._log.info("Test project behavior")

    # Set the input values you want to test
    # dut.ui_in.value = 20
    # dut.uio_in.value = 30

    # Wait for one clock cycle to see the output values
    # await ClockCycles(dut.clk, 1)

    # The following assersion is just an example of how to check the output values.
    # Change it to match the actual expected output of your module:
    # TODO
    # assert dut.uo_out.value == 50

    # Keep testing the module by changing the input values, waiting for
    # one or more clock cycles, and asserting the expected output values.

    proj = dut
    hsync_start = FallingEdge(proj.hsync)
    hsync_end = RisingEdge(proj.hsync)
    vsync_start = FallingEdge(proj.vsync)
    vsync_end = RisingEdge(proj.vsync)

    if proj.vsync.value != 1:
        await vsync_end
    if proj.hsync.value != 1:
        await hsync_end

    h0 = time_us()
    v0 = time_ms()

    dut._log.info("hsync: %d", hsync_start.signal.value)
    await wait_cycles(dut, clock, 656, hsync_start)
    dut._log.info("hsync: %d", hsync_start.signal.value)
    h1 = time_us()
    dut._log.info("hsync start: %fus", h1 - h0)

    await wait_cycles(dut, clock, 96, hsync_end)
    dut._log.info("hsync: %d", hsync_end.signal.value)
    h2 = time_us()
    dut._log.info("hsync duration: %fus", h2 - h1)

    await ClockCycles(dut.clk, 48)
    dut._log.info("line: %fus", time_us() - h0)

    await wait_cycles(dut, clock, (490 - 1) * 800 - 96 - 48, vsync_start)
    v1 = time_ms()
    dut._log.info("vsync start: %fms", v1 - v0)
    await wait_cycles(dut, clock, 2 * 800, vsync_end)

    v2 = time_ms()
    dut._log.info("vsync duration: %fms", v2 - v1)
    await ClockCycles(dut.clk, 33 * 800 + 96 + 48)
    dut._log.info("frame: %fms", time_ms() - v0)

    await hsync_start
    await vsync_start
    #    await vsync_end
    await wait_cycles(dut, clock, 525 * 800, vsync_start)
    await vsync_end
    await ClockCycles(dut.clk, 33 * 800)
