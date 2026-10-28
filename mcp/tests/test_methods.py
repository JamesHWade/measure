"""Executable chromatography method validation and canonicalization."""

import copy

import pytest
from pydantic import ValidationError

import methods  # noqa: E402


def absolute_method(**changes):
    data = {
        "schema_version": "1.0",
        "time_unit": "s",
        "flow_unit": "uL/min",
        "solvents": ["A", "B"],
        "program": {
            "kind": "gradient_absolute",
            "points": [
                {
                    "time": 0,
                    "state": {"flow": 1000, "solvent_fractions": {"A": 0.5, "B": 0.5}},
                },
                {
                    "time": 60,
                    "state": {"flow": 1000, "solvent_fractions": {"A": 0.5, "B": 0.5}},
                },
            ],
        },
    }
    data.update(changes)
    return data


def test_absolute_gradient_normalizes_units_and_preserves_analytical_zero():
    result = methods.evaluate_method(absolute_method())

    assert result.declaration.time_unit == "s"
    assert result.canonical.time_unit == "min"
    assert result.canonical.flow_unit == "mL/min"
    assert result.canonical.acquisition_duration_min == pytest.approx(1)
    assert result.canonical.points[0].time_min == 0
    assert result.canonical.points[1].time_min == pytest.approx(1)
    assert result.canonical.analytical_solvent_use_ml == {
        "A": pytest.approx(0.5),
        "B": pytest.approx(0.5),
    }
    assert result.feasible


def test_duration_gradient_is_canonical_equivalent_and_evaluation_round_trips():
    duration = absolute_method(
        program={
            "kind": "gradient_duration",
            "initial_state": {"flow": 1000, "solvent_fractions": {"A": 0.5, "B": 0.5}},
            "segments": [
                {
                    "duration": 60,
                    "end_state": {
                        "flow": 1000,
                        "solvent_fractions": {"A": 0.5, "B": 0.5},
                    },
                }
            ],
        }
    )

    absolute_result = methods.evaluate_method(absolute_method())
    duration_result = methods.evaluate_method(duration)

    assert duration_result.canonical == absolute_result.canonical
    restored = methods.MethodEvaluation.model_validate(
        duration_result.model_dump(mode="json")
    )
    assert restored == duration_result
    assert restored.declaration.program.kind == "gradient_duration"
    inconsistent = duration_result.model_dump(mode="json")
    inconsistent["feasible"] = False
    with pytest.raises(ValidationError):
        methods.MethodEvaluation.model_validate(inconsistent)


def test_fractional_duration_schedules_match_absolute_times_across_s_and_min():
    states = [
        {"flow": 1, "solvent_fractions": {"A": 0.2, "B": 0.8}},
        {"flow": 2, "solvent_fractions": {"A": 0.5, "B": 0.5}},
        {"flow": 3, "solvent_fractions": {"A": 0.8, "B": 0.2}},
    ]
    absolute = absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "gradient_absolute",
            "points": [
                {"time": 0, "state": states[0]},
                {"time": 0.1, "state": states[1]},
                {"time": 0.3, "state": states[2]},
            ],
        },
    )
    duration_minutes = absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "gradient_duration",
            "initial_state": states[0],
            "segments": [
                {"duration": 0.1, "end_state": states[1]},
                {"duration": 0.2, "end_state": states[2]},
            ],
        },
    )
    duration_seconds = absolute_method(
        time_unit="s",
        flow_unit="uL/min",
        program={
            "kind": "gradient_duration",
            "initial_state": {
                "flow": 1000,
                "solvent_fractions": states[0]["solvent_fractions"],
            },
            "segments": [
                {
                    "duration": 6,
                    "end_state": {
                        "flow": 2000,
                        "solvent_fractions": states[1]["solvent_fractions"],
                    },
                },
                {
                    "duration": 12,
                    "end_state": {
                        "flow": 3000,
                        "solvent_fractions": states[2]["solvent_fractions"],
                    },
                },
            ],
        },
    )

    expected = methods.evaluate_method(absolute).canonical

    assert methods.evaluate_method(duration_minutes).canonical == expected
    assert methods.evaluate_method(duration_seconds).canonical == expected


def test_isocratic_methods_match_across_time_and_flow_units():
    per_minute = absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "isocratic",
            "acquisition_duration": 0.3,
            "state": {"flow": 0.5, "solvent_fractions": {"A": 0.6, "B": 0.4}},
        },
    )
    per_second = absolute_method(
        time_unit="s",
        flow_unit="uL/min",
        program={
            "kind": "isocratic",
            "acquisition_duration": 18,
            "state": {"flow": 500, "solvent_fractions": {"A": 0.6, "B": 0.4}},
        },
    )

    assert (
        methods.evaluate_method(per_second).canonical
        == methods.evaluate_method(per_minute).canonical
    )


def test_linear_flow_and_fraction_integral_is_exact_and_overheads_keep_t_zero():
    declaration = absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "gradient_absolute",
            "points": [
                {
                    "time": 0,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.2, "B": 0.8}},
                },
                {
                    "time": 2,
                    "state": {"flow": 3, "solvent_fractions": {"A": 0.8, "B": 0.2}},
                },
            ],
        },
        pre_acquisition={
            "duration": 0.5,
            "state": {"flow": 0.5, "solvent_fractions": {"A": 0.8, "B": 0.2}},
        },
        post_acquisition={
            "duration": 1,
            "state": {"flow": 1, "solvent_fractions": {"A": 0.1, "B": 0.9}},
        },
    )

    result = methods.evaluate_method(declaration)

    assert result.canonical.points[0].time_min == 0
    assert result.canonical.points[1].time_min == 2
    assert result.canonical.acquisition_duration_min == pytest.approx(2)
    assert result.canonical.pre_acquisition_duration_min == pytest.approx(0.5)
    assert result.canonical.post_acquisition_duration_min == pytest.approx(1)
    assert result.canonical.occupancy_duration_min == pytest.approx(3.5)
    assert result.canonical.analytical_solvent_use_ml == {
        "A": pytest.approx(2.2),
        "B": pytest.approx(1.8),
    }
    assert result.canonical.pre_acquisition.solvent_use_ml == {
        "A": pytest.approx(0.2),
        "B": pytest.approx(0.05),
    }
    assert result.canonical.post_acquisition.solvent_use_ml == {
        "A": pytest.approx(0.1),
        "B": pytest.approx(0.9),
    }
    assert result.canonical.total_solvent_use_ml == {
        "A": pytest.approx(2.5),
        "B": pytest.approx(2.75),
    }


@pytest.mark.parametrize(
    "bad_value", [True, "1000", float("nan"), float("inf"), 10**400]
)
def test_numeric_fields_reject_booleans_strings_and_nonfinite_values(bad_value):
    payload = absolute_method()
    payload["program"]["points"][0]["state"]["flow"] = bad_value

    with pytest.raises(ValidationError):
        methods.MethodDeclaration.model_validate(payload)


def test_malformed_method_declarations_are_rejected():
    invalid = []

    payload = absolute_method()
    payload["unexpected"] = "field"
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["unexpected"] = "field"
    invalid.append(payload)

    payload = absolute_method()
    payload["schema_version"] = "2.0"
    invalid.append(payload)

    payload = absolute_method()
    payload["time_unit"] = "hours"
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["time"] = 0.01
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["time"] = -1
    invalid.append(payload)

    for time in (0, 70):
        payload = absolute_method()
        state = copy.deepcopy(payload["program"]["points"][0]["state"])
        payload["program"]["points"] = [
            {"time": 0, "state": state},
            {"time": time, "state": state},
            {"time": 60, "state": state},
        ]
        invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["flow"] = 0
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["flow"] = -1
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["solvent_fractions"] = {"A": 0.4, "B": 0.5}
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["solvent_fractions"] = {"A": 1.0}
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["solvent_fractions"] = {
        "A": 0.5,
        "B": 0.25,
        "C": 0.25,
    }
    invalid.append(payload)

    payload = absolute_method()
    payload["solvents"] = ["A", "A"]
    invalid.append(payload)

    payload = absolute_method()
    payload["solvents"] = [" ", "B"]
    invalid.append(payload)

    payload = absolute_method()
    payload["program"]["points"][0]["state"]["solvent_fractions"]["A"] = True
    invalid.append(payload)

    payload = absolute_method(
        program={
            "kind": "isocratic",
            "acquisition_duration": 0,
            "state": {"flow": 1, "solvent_fractions": {"A": 0.5, "B": 0.5}},
        }
    )
    invalid.append(payload)

    payload = absolute_method(
        program={
            "kind": "gradient_duration",
            "initial_state": {"flow": 1, "solvent_fractions": {"A": 0.5, "B": 0.5}},
            "segments": [
                {
                    "duration": 0,
                    "end_state": {"flow": 1, "solvent_fractions": {"A": 0.5, "B": 0.5}},
                }
            ],
        }
    )
    invalid.append(payload)

    payload = absolute_method(
        program={
            "kind": "gradient_duration",
            "initial_state": {"flow": 1, "solvent_fractions": {"A": 0.5, "B": 0.5}},
            "segments": [
                {
                    "duration": -1,
                    "end_state": {"flow": 1, "solvent_fractions": {"A": 0.5, "B": 0.5}},
                }
            ],
        }
    )
    invalid.append(payload)

    for payload in invalid:
        with pytest.raises(ValidationError):
            methods.MethodDeclaration.model_validate(payload)


def test_inconsistent_constraint_limits_are_rejected():
    with pytest.raises(ValidationError):
        methods.MethodConstraints.model_validate(
            {
                "min_segment_duration_min": 2,
                "max_acquisition_duration_min": 1,
            }
        )
    with pytest.raises(ValidationError):
        methods.MethodConstraints.model_validate(
            {
                "solvent_bounds": [
                    {"solvent_id": "A", "min_fraction": 0.8, "max_fraction": 0.2}
                ],
            }
        )
    with pytest.raises(ValidationError):
        methods.MethodConstraints.model_validate(
            {
                "max_flow_ml_min": True,
            }
        )
    with pytest.raises(ValidationError):
        methods.MethodConstraints.model_validate({"max_flow": 1})
    with pytest.raises(ValidationError):
        methods.MethodConstraints.model_validate(
            {
                "equipment_constraints": [
                    {"component": "column", "allowed_references": [" "]}
                ],
            }
        )


def varying_gradient(**changes):
    return absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "gradient_absolute",
            "points": [
                {
                    "time": 0,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.2, "B": 0.8}},
                },
                {
                    "time": 2,
                    "state": {"flow": 3, "solvent_fractions": {"A": 0.8, "B": 0.2}},
                },
            ],
        },
        **changes,
    )


def test_feasible_candidate_passes_exact_constraint_boundaries():
    constraints = {
        "min_segment_duration_min": 2,
        "max_acquisition_duration_min": 2,
        "max_occupancy_duration_min": 2,
        "max_flow_ml_min": 3,
        "max_abs_fraction_slope_per_min": 0.3,
        "solvent_bounds": [
            {
                "solvent_id": "A",
                "min_fraction": 0.2,
                "max_fraction": 0.8,
                "min_total_use_ml": 2.2,
                "max_total_use_ml": 2.2,
            },
            {
                "solvent_id": "B",
                "min_fraction": 0.2,
                "max_fraction": 0.8,
                "min_total_use_ml": 1.8,
                "max_total_use_ml": 1.8,
            },
        ],
        "equipment_constraints": [
            {"component": "instrument", "allowed_references": ["LC-1"]},
        ],
    }

    result = methods.evaluate_method(
        varying_gradient(
            instrument={"reference_id": "LC-1", "compatibility": "compatible"},
        ),
        constraints,
    )

    assert result.feasible
    assert result.violations == []


def test_infeasible_candidate_reports_bounded_values_limits_units_and_categories():
    declaration = varying_gradient(
        column={"reference_id": "C18", "compatibility": "incompatible"},
        instrument={"reference_id": "LC-1", "compatibility": "unknown"},
    )
    declaration["program"]["points"].insert(
        1,
        {
            "time": 1,
            "state": {"flow": 2, "solvent_fractions": {"A": 0.5, "B": 0.5}},
        },
    )
    constraints = {
        "min_segment_duration_min": 1.5,
        "max_acquisition_duration_min": 1.5,
        "max_occupancy_duration_min": 1.5,
        "max_flow_ml_min": 0.5,
        "max_abs_fraction_slope_per_min": 0.1,
        "solvent_bounds": [
            {
                "solvent_id": "A",
                "min_fraction": 0.3,
                "max_fraction": 0.7,
                "max_total_use_ml": 2,
            },
            {
                "solvent_id": "B",
                "min_fraction": 0.3,
                "max_fraction": 0.7,
                "min_total_use_ml": 2,
            },
        ],
        "equipment_constraints": [
            {"component": "column", "required": True, "allowed_references": ["Phenyl"]},
            {"component": "instrument", "allowed_references": ["LC-1"]},
            {"component": "detector", "required": True},
        ],
    }

    result = methods.evaluate_method(declaration, constraints)
    by_code = {violation.code: violation for violation in result.violations}

    assert not result.feasible
    assert len(result.violations) <= methods.MAX_VIOLATIONS
    assert {
        "min_segment_duration_exceeded",
        "max_acquisition_duration_exceeded",
        "max_occupancy_duration_exceeded",
        "max_flow_exceeded",
        "fraction_slope_exceeded",
        "fraction_below_minimum",
        "fraction_above_maximum",
        "solvent_use_below_minimum",
        "solvent_use_above_maximum",
        "equipment_reference_incompatible",
        "equipment_reference_not_allowed",
        "equipment_reference_required",
    }.issubset(by_code)
    assert by_code["min_segment_duration_exceeded"].value == pytest.approx(1)
    assert by_code["min_segment_duration_exceeded"].limit == pytest.approx(1.5)
    assert by_code["min_segment_duration_exceeded"].unit == "min"
    assert by_code["max_flow_exceeded"].unit == "mL/min"
    assert by_code["fraction_slope_exceeded"].unit == "1/min"
    assert by_code["solvent_use_above_maximum"].unit == "mL"
    assert by_code["equipment_reference_not_allowed"].value == "C18"
    assert by_code["equipment_reference_not_allowed"].limit == ["Phenyl"]
    assert by_code["equipment_reference_required"].equipment_component == "detector"
    assert by_code["equipment_reference_required"].value is None
    assert by_code["equipment_reference_required"].limit == "required"
    assert (
        by_code["equipment_reference_incompatible"].unit
        == "caller_declared_compatibility"
    )
    a_fraction_violation = next(
        item
        for item in result.violations
        if item.code == "fraction_below_minimum" and item.solvent_id == "A"
    )
    assert a_fraction_violation.phase == "analysis"
    assert a_fraction_violation.point_index == 0


def test_method_constraints_reject_unknown_solvents_and_impossible_mixtures():
    unknown = {"solvent_bounds": [{"solvent_id": "C", "max_fraction": 0.5}]}
    impossible_minimum = {
        "solvent_bounds": [
            {"solvent_id": "A", "min_fraction": 0.7},
            {"solvent_id": "B", "min_fraction": 0.7},
        ]
    }
    impossible_maximum = {
        "solvent_bounds": [
            {"solvent_id": "A", "max_fraction": 0.3},
            {"solvent_id": "B", "max_fraction": 0.4},
        ]
    }

    for constraints in (unknown, impossible_minimum, impossible_maximum):
        with pytest.raises(ValidationError):
            methods.evaluate_method(absolute_method(), constraints)


def test_composition_can_increase_decrease_and_hold_without_global_monotonicity_rule():
    declaration = absolute_method(
        time_unit="min",
        flow_unit="mL/min",
        program={
            "kind": "gradient_absolute",
            "points": [
                {
                    "time": 0,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.2, "B": 0.8}},
                },
                {
                    "time": 1,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.9, "B": 0.1}},
                },
                {
                    "time": 2,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.3, "B": 0.7}},
                },
                {
                    "time": 3,
                    "state": {"flow": 1, "solvent_fractions": {"A": 0.3, "B": 0.7}},
                },
            ],
        },
    )

    result = methods.evaluate_method(declaration)

    assert result.feasible
    assert [point.solvent_fractions["A"] for point in result.canonical.points] == [
        0.2,
        0.9,
        0.3,
        0.3,
    ]


def test_isocratic_method_has_explicit_acquisition_duration_and_segment_bounds():
    declaration = absolute_method(
        program={
            "kind": "isocratic",
            "acquisition_duration": 120,
            "state": {"flow": 500, "solvent_fractions": {"A": 0.6, "B": 0.4}},
        },
    )

    result = methods.evaluate_method(
        declaration,
        {
            "min_segment_duration_min": 2,
            "max_acquisition_duration_min": 2,
            "max_flow_ml_min": 0.5,
        },
    )

    assert result.feasible
    assert result.canonical.program_kind == "isocratic"
    assert [point.time_min for point in result.canonical.points] == [0, 2]
    assert result.canonical.analytical_solvent_use_ml == {
        "A": pytest.approx(0.6),
        "B": pytest.approx(0.4),
    }
