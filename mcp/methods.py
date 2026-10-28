"""Pure validation and canonicalization for executable LC methods.

This module describes bounded candidate methods; it does not execute an
instrument, persist records, sample an optimizer space, or qualify pressure.
"""

from __future__ import annotations

import math
from decimal import Decimal, localcontext
from typing import Annotated, Any, Literal, TypeAlias

from pydantic import (
    BeforeValidator,
    BaseModel,
    ConfigDict,
    Field,
    StringConstraints,
    StrictStr,
    field_validator,
    model_validator,
)


METHOD_SCHEMA_VERSION = "1.0"
MAX_SOLVENTS = 6
MAX_GRADIENT_POINTS = 16
MAX_METHOD_BYTES = 64_000
MAX_CONSTRAINTS_BYTES = 32_000
# Worst case is below this bound: 15 segment, 18 flow, 90 slope, 108
# fraction, 12 solvent-use, 6 equipment, plus acquisition/occupancy violations.
MAX_VIOLATIONS = 256
_SUM_TOLERANCE = 1e-9


def _number(value: Any) -> float:
    """Accept finite JSON-style numbers while rejecting bools and strings."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(
            "Expected a finite numeric value; booleans and strings are not numeric"
        )
    try:
        result = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError("Numeric value is outside the supported finite range") from exc
    if not math.isfinite(result):
        raise ValueError("Numeric values must be finite")
    return result


FiniteNumber: TypeAlias = Annotated[float, BeforeValidator(_number)]
NonnegativeNumber: TypeAlias = Annotated[FiniteNumber, Field(ge=0)]
PositiveNumber: TypeAlias = Annotated[FiniteNumber, Field(gt=0)]
Fraction: TypeAlias = Annotated[FiniteNumber, Field(ge=0, le=1)]
Identifier: TypeAlias = Annotated[str, StringConstraints(min_length=1, max_length=100)]
SolventIdentifier: TypeAlias = Annotated[
    str, StringConstraints(min_length=1, max_length=32)
]
TimeUnit = Literal["s", "min"]
FlowUnit = Literal["uL/min", "mL/min"]
EquipmentComponent = Literal["column", "instrument", "detector"]
PhaseName = Literal["pre_acquisition", "analysis", "post_acquisition"]


class StrictModel(BaseModel):
    model_config = ConfigDict(
        extra="forbid", strict=True, allow_inf_nan=False, validate_default=True
    )

    @field_validator("*")
    @classmethod
    def reject_whitespace_only_strings(cls, value: Any) -> Any:
        if isinstance(value, str) and value and not value.strip():
            raise ValueError("Text values must not consist only of whitespace")
        return value


class GradientState(StrictModel):
    flow: PositiveNumber = Field(
        description="Declared positive flow in the method's flow_unit."
    )
    solvent_fractions: dict[SolventIdentifier, Fraction] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )

    @model_validator(mode="after")
    def validate_mixture(self) -> GradientState:
        if any(not key.strip() for key in self.solvent_fractions):
            raise ValueError("Solvent identifiers must not be blank")
        total = math.fsum(self.solvent_fractions.values())
        if not math.isclose(total, 1.0, rel_tol=0.0, abs_tol=_SUM_TOLERANCE):
            raise ValueError("Solvent fractions must sum to 1 within 1e-9")
        return self


class IsocraticProgram(StrictModel):
    kind: Literal["isocratic"]
    acquisition_duration: PositiveNumber = Field(
        description="Acquisition duration in the method's time_unit."
    )
    state: GradientState


class AbsoluteGradientPoint(StrictModel):
    time: NonnegativeNumber = Field(
        description="Absolute analytical time from t=0 in the method's time_unit."
    )
    state: GradientState


class AbsoluteGradientProgram(StrictModel):
    kind: Literal["gradient_absolute"]
    points: list[AbsoluteGradientPoint] = Field(
        min_length=2, max_length=MAX_GRADIENT_POINTS
    )

    @model_validator(mode="after")
    def validate_absolute_timeline(self) -> AbsoluteGradientProgram:
        times = [point.time for point in self.points]
        if times[0] != 0:
            raise ValueError(
                "The first absolute gradient time must be exactly 0 at analytical t=0"
            )
        if any(current <= previous for previous, current in zip(times, times[1:])):
            raise ValueError(
                "Absolute gradient times must be strictly increasing and unique"
            )
        return self


class DurationGradientSegment(StrictModel):
    duration: PositiveNumber = Field(
        description="Positive duration from the previous state in the method's time_unit."
    )
    end_state: GradientState


class DurationGradientProgram(StrictModel):
    kind: Literal["gradient_duration"]
    initial_state: GradientState = Field(
        description="State at analytical t=0; its time is never inferred."
    )
    segments: list[DurationGradientSegment] = Field(
        min_length=1, max_length=MAX_GRADIENT_POINTS - 1
    )


class OverheadPhase(StrictModel):
    duration: PositiveNumber = Field(
        description="Positive phase duration in the method's time_unit."
    )
    state: GradientState


class EquipmentReference(StrictModel):
    reference_id: Identifier
    compatibility: Literal["compatible", "incompatible", "unknown"] = Field(
        default="unknown",
        description="Caller-declared compatibility; this service does not verify equipment metadata.",
    )


Program: TypeAlias = Annotated[
    IsocraticProgram | AbsoluteGradientProgram | DurationGradientProgram,
    Field(discriminator="kind"),
]


class MethodDeclaration(StrictModel):
    schema_version: Literal["1.0"] = METHOD_SCHEMA_VERSION
    time_unit: TimeUnit
    flow_unit: FlowUnit
    solvents: list[SolventIdentifier] = Field(min_length=1, max_length=MAX_SOLVENTS)
    program: Program
    pre_acquisition: OverheadPhase | None = None
    post_acquisition: OverheadPhase | None = None
    column: EquipmentReference | None = None
    instrument: EquipmentReference | None = None
    detector: EquipmentReference | None = None

    @model_validator(mode="after")
    def validate_method_declaration(self) -> MethodDeclaration:
        if any(not solvent.strip() for solvent in self.solvents):
            raise ValueError("Solvent identifiers must not be blank")
        if len(set(self.solvents)) != len(self.solvents):
            raise ValueError("Solvent identifiers must be unique")
        allowed = set(self.solvents)
        for state in _method_states(self):
            if set(state.solvent_fractions) != allowed:
                raise ValueError(
                    "Every method state must declare exactly the method's solvent identifiers"
                )
        return self


class SolventBounds(StrictModel):
    solvent_id: SolventIdentifier
    min_fraction: Fraction | None = None
    max_fraction: Fraction | None = None
    min_total_use_ml: NonnegativeNumber | None = None
    max_total_use_ml: NonnegativeNumber | None = None

    @model_validator(mode="after")
    def validate_bounds(self) -> SolventBounds:
        if all(
            value is None
            for value in (
                self.min_fraction,
                self.max_fraction,
                self.min_total_use_ml,
                self.max_total_use_ml,
            )
        ):
            raise ValueError(
                "A solvent bound must declare at least one fraction or total-use limit"
            )
        if (
            self.min_fraction is not None
            and self.max_fraction is not None
            and self.min_fraction > self.max_fraction
        ):
            raise ValueError("min_fraction must not exceed max_fraction")
        if (
            self.min_total_use_ml is not None
            and self.max_total_use_ml is not None
            and self.min_total_use_ml > self.max_total_use_ml
        ):
            raise ValueError("min_total_use_ml must not exceed max_total_use_ml")
        return self


class EquipmentConstraint(StrictModel):
    component: EquipmentComponent
    required: bool = False
    allowed_references: list[Identifier] | None = Field(
        default=None, min_length=1, max_length=32
    )

    @model_validator(mode="after")
    def validate_references(self) -> EquipmentConstraint:
        if self.allowed_references is not None:
            if any(not reference.strip() for reference in self.allowed_references):
                raise ValueError("Allowed equipment references must not be blank")
            if len(set(self.allowed_references)) != len(self.allowed_references):
                raise ValueError("Allowed equipment references must be unique")
        return self


class MethodConstraints(StrictModel):
    min_segment_duration_min: PositiveNumber | None = None
    max_acquisition_duration_min: PositiveNumber | None = None
    max_occupancy_duration_min: PositiveNumber | None = None
    max_flow_ml_min: PositiveNumber | None = None
    max_abs_fraction_slope_per_min: NonnegativeNumber | None = None
    solvent_bounds: list[SolventBounds] = Field(
        default_factory=list, max_length=MAX_SOLVENTS
    )
    equipment_constraints: list[EquipmentConstraint] = Field(
        default_factory=list, max_length=3
    )

    @model_validator(mode="after")
    def validate_constraint_set(self) -> MethodConstraints:
        if self.min_segment_duration_min is not None:
            for field_name in (
                "max_acquisition_duration_min",
                "max_occupancy_duration_min",
            ):
                maximum = getattr(self, field_name)
                if maximum is not None and self.min_segment_duration_min > maximum:
                    raise ValueError(
                        f"min_segment_duration_min cannot exceed {field_name}"
                    )
        solvent_ids = [bound.solvent_id for bound in self.solvent_bounds]
        if len(set(solvent_ids)) != len(solvent_ids):
            raise ValueError("Each solvent may have only one bounds entry")
        components = [item.component for item in self.equipment_constraints]
        if len(set(components)) != len(components):
            raise ValueError(
                "Each equipment component may have only one compatibility constraint"
            )
        return self


class CanonicalPoint(StrictModel):
    time_min: NonnegativeNumber
    flow_ml_min: PositiveNumber
    solvent_fractions: dict[SolventIdentifier, Fraction] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )


class CanonicalPhase(StrictModel):
    duration_min: PositiveNumber
    flow_ml_min: PositiveNumber
    solvent_fractions: dict[SolventIdentifier, Fraction] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )
    solvent_use_ml: dict[SolventIdentifier, NonnegativeNumber] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )


class CanonicalMethod(StrictModel):
    schema_version: Literal["1.0"] = METHOD_SCHEMA_VERSION
    time_unit: Literal["min"] = "min"
    flow_unit: Literal["mL/min"] = "mL/min"
    volume_unit: Literal["mL"] = "mL"
    program_kind: Literal["isocratic", "gradient"]
    acquisition_duration_min: PositiveNumber
    pre_acquisition_duration_min: NonnegativeNumber
    post_acquisition_duration_min: NonnegativeNumber
    occupancy_duration_min: PositiveNumber
    points: list[CanonicalPoint] = Field(min_length=2, max_length=MAX_GRADIENT_POINTS)
    pre_acquisition: CanonicalPhase | None = None
    post_acquisition: CanonicalPhase | None = None
    analytical_solvent_use_ml: dict[SolventIdentifier, NonnegativeNumber] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )
    total_solvent_use_ml: dict[SolventIdentifier, NonnegativeNumber] = Field(
        min_length=1, max_length=MAX_SOLVENTS
    )


class MethodViolation(StrictModel):
    code: Identifier
    scope: Literal["method", "segment", "solvent", "phase", "equipment"]
    phase: PhaseName | None = None
    segment_index: int | None = Field(default=None, ge=0)
    point_index: int | None = Field(default=None, ge=0)
    solvent_id: SolventIdentifier | None = None
    equipment_component: EquipmentComponent | None = None
    value: FiniteNumber | StrictStr | None
    limit: FiniteNumber | StrictStr | list[StrictStr] | None
    unit: Identifier


class MethodEvaluation(StrictModel):
    schema_version: Literal["1.0"] = METHOD_SCHEMA_VERSION
    declaration: MethodDeclaration
    constraints: MethodConstraints | None = None
    canonical: CanonicalMethod
    feasible: bool
    violations: list[MethodViolation] = Field(max_length=MAX_VIOLATIONS)

    @model_validator(mode="after")
    def validate_evaluation(self) -> MethodEvaluation:
        if self.feasible != (len(self.violations) == 0):
            raise ValueError(
                "feasible must agree with whether the violations list is empty"
            )
        expected_kind = (
            "isocratic"
            if isinstance(self.declaration.program, IsocraticProgram)
            else "gradient"
        )
        if self.canonical.program_kind != expected_kind:
            raise ValueError("Canonical program_kind must agree with the declaration")
        return self


class _MethodEvaluationInput(StrictModel):
    declaration: MethodDeclaration
    constraints: MethodConstraints | None = None

    @model_validator(mode="after")
    def validate_constraint_space(self) -> _MethodEvaluationInput:
        if self.constraints is None:
            return self
        solvent_ids = set(self.declaration.solvents)
        constrained_ids = {
            bound.solvent_id for bound in self.constraints.solvent_bounds
        }
        unknown = sorted(constrained_ids - solvent_ids)
        if unknown:
            raise ValueError(
                f"Solvent constraints refer to undeclared solvent identifiers: {unknown}"
            )
        bounds = {bound.solvent_id: bound for bound in self.constraints.solvent_bounds}
        minimum_total = math.fsum(
            0.0
            if solvent not in bounds or bounds[solvent].min_fraction is None
            else bounds[solvent].min_fraction
            for solvent in self.declaration.solvents
        )
        maximum_total = math.fsum(
            1.0
            if solvent not in bounds or bounds[solvent].max_fraction is None
            else bounds[solvent].max_fraction
            for solvent in self.declaration.solvents
        )
        if minimum_total > 1.0 + _SUM_TOLERANCE or maximum_total < 1.0 - _SUM_TOLERANCE:
            raise ValueError("Solvent fraction limits cannot sum to a mixture of 1")
        return self


def _method_states(method: MethodDeclaration) -> list[GradientState]:
    program = method.program
    if isinstance(program, IsocraticProgram):
        states = [program.state]
    elif isinstance(program, AbsoluteGradientProgram):
        states = [point.state for point in program.points]
    else:
        states = [
            program.initial_state,
            *(segment.end_state for segment in program.segments),
        ]
    if method.pre_acquisition is not None:
        states.append(method.pre_acquisition.state)
    if method.post_acquisition is not None:
        states.append(method.post_acquisition.state)
    return states


def _finite_result(value: float, context: str) -> float:
    if not math.isfinite(value):
        raise ValueError(
            f"Canonical {context} exceeds the supported finite numeric range"
        )
    return value


def _finite_sum(values: Any, context: str) -> float:
    try:
        total = math.fsum(values)
    except OverflowError as exc:
        raise ValueError(
            f"Canonical {context} exceeds the supported finite numeric range"
        ) from exc
    return _finite_result(total, context)


def _time_to_min(value: float, unit: TimeUnit) -> float:
    with localcontext() as context:
        context.prec = 50
        return _decimal_time_to_min(Decimal(str(value)), unit)


def _cumulative_times_to_min(durations: Any, unit: TimeUnit) -> list[float]:
    """Accumulate declared durations before conversion to canonical minutes."""
    with localcontext() as context:
        context.prec = 50
        elapsed = Decimal(0)
        times = [0.0]
        for duration in durations:
            elapsed += Decimal(str(duration))
            times.append(_decimal_time_to_min(elapsed, unit))
    if any(current <= previous for previous, current in zip(times, times[1:])):
        raise ValueError(
            "Canonical gradient segment times must remain strictly increasing"
        )
    return times


def _decimal_time_to_min(value: Decimal, unit: TimeUnit) -> float:
    converted = value / Decimal(60) if unit == "s" else value
    if converted.is_zero():
        return 0.0
    try:
        as_float = float(converted)
    except (OverflowError, ValueError) as exc:
        raise ValueError(
            "Canonical time exceeds the supported finite numeric range"
        ) from exc
    return _finite_result(as_float, "time")


def _flow_to_ml_min(value: float, unit: FlowUnit) -> float:
    with localcontext() as context:
        context.prec = 50
        declared = Decimal(str(value))
        converted = float(declared / Decimal(1000)) if unit == "uL/min" else value
    if converted <= 0:
        raise ValueError("Canonical flow must remain positive after unit conversion")
    return _finite_result(converted, "flow")


def _canonical_point(
    time_min: float, state: GradientState, method: MethodDeclaration
) -> CanonicalPoint:
    return CanonicalPoint(
        time_min=time_min,
        flow_ml_min=_flow_to_ml_min(state.flow, method.flow_unit),
        solvent_fractions=_canonical_fractions(state),
    )


def _canonical_fractions(state: GradientState) -> dict[str, float]:
    return {
        solvent: 0.0 if fraction == 0.0 else fraction
        for solvent, fraction in state.solvent_fractions.items()
    }


def _linear_product_volume(
    duration_min: float,
    flow_start: float,
    flow_end: float,
    fraction_start: float,
    fraction_end: float,
) -> float:
    """Integrate the product of linearly varying flow and fraction exactly.

    Their product is quadratic, so Simpson's rule evaluates this integral
    exactly (up to floating-point rounding).
    """
    flow_mid = flow_start + (flow_end - flow_start) * 0.5
    fraction_mid = fraction_start + (fraction_end - fraction_start) * 0.5
    average_product = math.fsum(
        (
            (flow_start * fraction_start) / 6.0,
            (flow_mid * fraction_mid) * (2.0 / 3.0),
            (flow_end * fraction_end) / 6.0,
        )
    )
    return _finite_result(duration_min * average_product, "solvent use")


def _integrate_points(
    points: list[CanonicalPoint], solvents: list[str]
) -> dict[str, float]:
    by_solvent: dict[str, list[float]] = {solvent: [] for solvent in solvents}
    for start, end in zip(points, points[1:]):
        duration = end.time_min - start.time_min
        if duration <= 0:
            raise ValueError(
                "Canonical analytical point times must remain strictly increasing"
            )
        for solvent in solvents:
            by_solvent[solvent].append(
                _linear_product_volume(
                    duration,
                    start.flow_ml_min,
                    end.flow_ml_min,
                    start.solvent_fractions[solvent],
                    end.solvent_fractions[solvent],
                )
            )
    return {
        solvent: _finite_sum(volumes, "solvent-use total")
        for solvent, volumes in by_solvent.items()
    }


def _canonical_phase(
    phase: OverheadPhase | None, method: MethodDeclaration
) -> CanonicalPhase | None:
    if phase is None:
        return None
    duration_min = _time_to_min(phase.duration, method.time_unit)
    if duration_min <= 0:
        raise ValueError(
            "Canonical overhead duration must remain positive after unit conversion"
        )
    flow_ml_min = _flow_to_ml_min(phase.state.flow, method.flow_unit)
    fractions = _canonical_fractions(phase.state)
    use = {
        solvent: _finite_result(
            duration_min * (flow_ml_min * fraction), "overhead solvent use"
        )
        for solvent, fraction in fractions.items()
    }
    return CanonicalPhase(
        duration_min=duration_min,
        flow_ml_min=flow_ml_min,
        solvent_fractions=fractions,
        solvent_use_ml=use,
    )


def _canonical_points(method: MethodDeclaration) -> tuple[list[CanonicalPoint], float]:
    program = method.program
    if isinstance(program, IsocraticProgram):
        duration = _time_to_min(program.acquisition_duration, method.time_unit)
        if duration <= 0:
            raise ValueError(
                "Canonical acquisition duration must remain positive after unit conversion"
            )
        return [
            _canonical_point(0.0, program.state, method),
            _canonical_point(duration, program.state, method),
        ], duration
    if isinstance(program, AbsoluteGradientProgram):
        points = [
            _canonical_point(
                _time_to_min(point.time, method.time_unit), point.state, method
            )
            for point in program.points
        ]
        if any(
            current.time_min <= previous.time_min
            for previous, current in zip(points, points[1:])
        ):
            raise ValueError(
                "Canonical absolute gradient times must remain strictly increasing"
            )
        duration = points[-1].time_min
        if duration <= 0:
            raise ValueError("Canonical acquisition duration must remain positive")
        return points, duration

    times = _cumulative_times_to_min(
        (segment.duration for segment in program.segments), method.time_unit
    )
    points = [_canonical_point(times[0], program.initial_state, method)]
    points.extend(
        _canonical_point(time_min, segment.end_state, method)
        for time_min, segment in zip(times[1:], program.segments)
    )
    if times[-1] <= 0:
        raise ValueError("Canonical acquisition duration must remain positive")
    return points, times[-1]


def _canonical_method(method: MethodDeclaration) -> CanonicalMethod:
    points, acquisition_duration = _canonical_points(method)
    pre = _canonical_phase(method.pre_acquisition, method)
    post = _canonical_phase(method.post_acquisition, method)
    pre_duration = 0.0 if pre is None else pre.duration_min
    post_duration = 0.0 if post is None else post.duration_min
    occupancy = _finite_sum(
        (pre_duration, acquisition_duration, post_duration), "occupancy duration"
    )
    analytical_use = _integrate_points(points, method.solvents)
    total_use = {
        solvent: _finite_sum(
            (
                analytical_use[solvent],
                0.0 if pre is None else pre.solvent_use_ml[solvent],
                0.0 if post is None else post.solvent_use_ml[solvent],
            ),
            "total solvent use",
        )
        for solvent in method.solvents
    }
    return CanonicalMethod(
        program_kind="isocratic"
        if isinstance(method.program, IsocraticProgram)
        else "gradient",
        acquisition_duration_min=acquisition_duration,
        pre_acquisition_duration_min=pre_duration,
        post_acquisition_duration_min=post_duration,
        occupancy_duration_min=occupancy,
        points=points,
        pre_acquisition=pre,
        post_acquisition=post,
        analytical_solvent_use_ml=analytical_use,
        total_solvent_use_ml=total_use,
    )


def _below(value: float, limit: float) -> bool:
    return value < limit and not math.isclose(value, limit, rel_tol=1e-9, abs_tol=1e-12)


def _above(value: float, limit: float) -> bool:
    return value > limit and not math.isclose(value, limit, rel_tol=1e-9, abs_tol=1e-12)


def _method_violations(
    method: MethodDeclaration,
    canonical: CanonicalMethod,
    constraints: MethodConstraints | None,
) -> list[MethodViolation]:
    violations: list[MethodViolation] = []

    def add(
        code: str,
        scope: Literal["method", "segment", "solvent", "phase", "equipment"],
        value: float | str | None,
        limit: float | str | list[str] | None,
        unit: str,
        *,
        phase: PhaseName | None = None,
        segment_index: int | None = None,
        point_index: int | None = None,
        solvent_id: str | None = None,
        equipment_component: EquipmentComponent | None = None,
    ) -> None:
        violations.append(
            MethodViolation(
                code=code,
                scope=scope,
                phase=phase,
                segment_index=segment_index,
                point_index=point_index,
                solvent_id=solvent_id,
                equipment_component=equipment_component,
                value=value,
                limit=limit,
                unit=unit,
            )
        )

    if constraints is not None:
        if constraints.max_acquisition_duration_min is not None and _above(
            canonical.acquisition_duration_min, constraints.max_acquisition_duration_min
        ):
            add(
                "max_acquisition_duration_exceeded",
                "method",
                canonical.acquisition_duration_min,
                constraints.max_acquisition_duration_min,
                "min",
            )
        if constraints.max_occupancy_duration_min is not None and _above(
            canonical.occupancy_duration_min, constraints.max_occupancy_duration_min
        ):
            add(
                "max_occupancy_duration_exceeded",
                "method",
                canonical.occupancy_duration_min,
                constraints.max_occupancy_duration_min,
                "min",
            )
        if constraints.min_segment_duration_min is not None:
            for index, (start, end) in enumerate(
                zip(canonical.points, canonical.points[1:])
            ):
                duration = end.time_min - start.time_min
                if _below(duration, constraints.min_segment_duration_min):
                    add(
                        "min_segment_duration_exceeded",
                        "segment",
                        duration,
                        constraints.min_segment_duration_min,
                        "min",
                        phase="analysis",
                        segment_index=index,
                    )

        if constraints.max_flow_ml_min is not None:
            for point_index, point in enumerate(canonical.points):
                if _above(point.flow_ml_min, constraints.max_flow_ml_min):
                    add(
                        "max_flow_exceeded",
                        "phase",
                        point.flow_ml_min,
                        constraints.max_flow_ml_min,
                        "mL/min",
                        phase="analysis",
                        point_index=point_index,
                    )
            for phase_name, phase_data in (
                ("pre_acquisition", canonical.pre_acquisition),
                ("post_acquisition", canonical.post_acquisition),
            ):
                if phase_data is not None and _above(
                    phase_data.flow_ml_min, constraints.max_flow_ml_min
                ):
                    add(
                        "max_flow_exceeded",
                        "phase",
                        phase_data.flow_ml_min,
                        constraints.max_flow_ml_min,
                        "mL/min",
                        phase=phase_name,
                    )

        if constraints.max_abs_fraction_slope_per_min is not None:
            for index, (start, end) in enumerate(
                zip(canonical.points, canonical.points[1:])
            ):
                duration = end.time_min - start.time_min
                for solvent in method.solvents:
                    slope = _finite_result(
                        abs(
                            end.solvent_fractions[solvent]
                            - start.solvent_fractions[solvent]
                        )
                        / duration,
                        "fraction slope",
                    )
                    if _above(slope, constraints.max_abs_fraction_slope_per_min):
                        add(
                            "fraction_slope_exceeded",
                            "segment",
                            slope,
                            constraints.max_abs_fraction_slope_per_min,
                            "1/min",
                            phase="analysis",
                            segment_index=index,
                            solvent_id=solvent,
                        )

        for bounds in constraints.solvent_bounds:
            for phase_name, point_index, fractions in _phase_fractions(canonical):
                fraction = fractions[bounds.solvent_id]
                if bounds.min_fraction is not None and _below(
                    fraction, bounds.min_fraction
                ):
                    add(
                        "fraction_below_minimum",
                        "solvent",
                        fraction,
                        bounds.min_fraction,
                        "fraction",
                        phase=phase_name,
                        point_index=point_index,
                        solvent_id=bounds.solvent_id,
                    )
                if bounds.max_fraction is not None and _above(
                    fraction, bounds.max_fraction
                ):
                    add(
                        "fraction_above_maximum",
                        "solvent",
                        fraction,
                        bounds.max_fraction,
                        "fraction",
                        phase=phase_name,
                        point_index=point_index,
                        solvent_id=bounds.solvent_id,
                    )
            total_use = canonical.total_solvent_use_ml[bounds.solvent_id]
            if bounds.min_total_use_ml is not None and _below(
                total_use, bounds.min_total_use_ml
            ):
                add(
                    "solvent_use_below_minimum",
                    "solvent",
                    total_use,
                    bounds.min_total_use_ml,
                    "mL",
                    solvent_id=bounds.solvent_id,
                )
            if bounds.max_total_use_ml is not None and _above(
                total_use, bounds.max_total_use_ml
            ):
                add(
                    "solvent_use_above_maximum",
                    "solvent",
                    total_use,
                    bounds.max_total_use_ml,
                    "mL",
                    solvent_id=bounds.solvent_id,
                )

    equipment_constraints = (
        {}
        if constraints is None
        else {item.component: item for item in constraints.equipment_constraints}
    )
    for component in ("column", "instrument", "detector"):
        reference = getattr(method, component)
        rule = equipment_constraints.get(component)
        if reference is not None and reference.compatibility == "incompatible":
            add(
                "equipment_reference_incompatible",
                "equipment",
                "incompatible",
                "compatible",
                "caller_declared_compatibility",
                equipment_component=component,
            )
        if reference is None:
            if rule is not None and rule.required:
                add(
                    "equipment_reference_required",
                    "equipment",
                    None,
                    "required"
                    if rule.allowed_references is None
                    else rule.allowed_references,
                    "reference",
                    equipment_component=component,
                )
            continue
        if (
            rule is not None
            and rule.allowed_references is not None
            and reference.reference_id not in rule.allowed_references
        ):
            add(
                "equipment_reference_not_allowed",
                "equipment",
                reference.reference_id,
                rule.allowed_references,
                "reference",
                equipment_component=component,
            )

    if len(violations) > MAX_VIOLATIONS:
        raise RuntimeError("Internal violation bound exceeded")
    return violations


def _phase_fractions(
    canonical: CanonicalMethod,
) -> list[tuple[PhaseName, int | None, dict[str, float]]]:
    fractions: list[tuple[PhaseName, int | None, dict[str, float]]] = [
        ("analysis", index, point.solvent_fractions)
        for index, point in enumerate(canonical.points)
    ]
    if canonical.pre_acquisition is not None:
        fractions.append(
            ("pre_acquisition", None, canonical.pre_acquisition.solvent_fractions)
        )
    if canonical.post_acquisition is not None:
        fractions.append(
            ("post_acquisition", None, canonical.post_acquisition.solvent_fractions)
        )
    return fractions


def evaluate_method(
    declaration: MethodDeclaration | dict[str, Any],
    constraints: MethodConstraints | dict[str, Any] | None = None,
) -> MethodEvaluation:
    """Validate and canonicalize an executable LC method and assess its bounds."""
    method = (
        declaration
        if isinstance(declaration, MethodDeclaration)
        else MethodDeclaration.model_validate(declaration)
    )
    limits = (
        constraints
        if isinstance(constraints, MethodConstraints)
        else (
            None
            if constraints is None
            else MethodConstraints.model_validate(constraints)
        )
    )
    validated_input = _MethodEvaluationInput(declaration=method, constraints=limits)
    method = validated_input.declaration
    limits = validated_input.constraints
    if len(method.model_dump_json().encode("utf-8")) > MAX_METHOD_BYTES:
        raise ValueError("Method declaration exceeds the byte limit")
    if (
        limits is not None
        and len(limits.model_dump_json().encode("utf-8")) > MAX_CONSTRAINTS_BYTES
    ):
        raise ValueError("Method constraints exceed the byte limit")
    canonical = _canonical_method(method)
    violations = _method_violations(method, canonical, limits)
    return MethodEvaluation(
        declaration=method,
        constraints=limits,
        canonical=canonical,
        feasible=not violations,
        violations=violations,
    )
