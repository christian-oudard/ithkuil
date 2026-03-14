#!/usr/bin/env python3
"""Update affixes.json with descriptions and types from the affixes PDF."""
import json
import sys

# Ithkuil sibilants and consonant classes
SIBILANTS = set('cčsšzžẓj')
STOPS = set('pbtdkg')
FRICATIVES_FOR_0 = set('fvţḑļ')  # Non-sibilant fricatives (including lateral ļ)

def derive_type(cs):
    """Derive gradient type from Cs consonant form using phonological rules (affixes.pdf p.2)."""
    if cs == 'ň':
        return 'C'
    if cs == 'ř' or cs.endswith('ř'):
        return 'C'
    if cs == 'r':
        return 'D1'
    if len(cs) >= 2 and (cs[0] == 'r' or cs[0] == 'ř'):
        return 'A1'
    if cs.endswith('h'):
        return 'A2'
    if 'ç' in cs or 'pč' in cs:
        return 'B'
    if cs.endswith('x'):
        return 'D2'
    if len(cs) >= 2 and cs.endswith('m'):
        return 'D2'
    if len(cs) == 1 and cs in SIBILANTS:
        return '0'
    # Ends in sibilant, doesn't start with sibilant
    if len(cs) >= 2 and cs[-1] in SIBILANTS and cs[0] not in SIBILANTS:
        return '0'
    # Sibilant + stop
    if len(cs) >= 2 and cs[0] in SIBILANTS and len(cs) == 2 and cs[1] in STOPS:
        return '0'
    # Sibilant + non-sibilant fricative
    if len(cs) >= 2 and cs[0] in SIBILANTS and len(cs) == 2 and cs[1] in FRICATIVES_FOR_0:
        return '0'
    return 'D1'


# ============================================================================
# DESCRIPTIONS: Maps abbreviation -> full description name
# Extracted from all 140 pages of affixes.pdf
# ============================================================================
DESCRIPTIONS = {
    # Sec 4.1: Demonstrative, Determinative and Deictic Affixes
    "DX1": "Deixis Categories",
    "DX2": "Additional Deixis Categories",
    "IDF": "Identificative",
    "EXT": "Exactness/Comparability of Identity / Identity As a Standard",
    "FAM": "Degree of Familiarity",
    "SIM": "Degree of Similarity",

    # Sec 4.2: Sequential/Temporal Affixes
    "SEQ": "Numerical Sequence",
    "SQT": "Sequence Relative to Present Context",
    "TNX": "Multiples of Ten",
    "ELA": "Elapsed Time",
    "SQC": "Sequence of Cause and Effect",
    "ASQ": "Aspectual Sequencing",
    "TPI": "Impact or Duration Over Time",
    "TD1": "Temporary Duration 1",
    "TD2": "Temporary Duration 2",
    "LTD": "Life-Time Duration",
    "LGD": "Long-Term Duration",
    "EPC": "Epoch-Length Duration",
    "UST": "Ultra-Short-Term Temporal Scale",
    "STT": "Short-Term Time References",
    "ST2": "Short-Term Time References 2",
    "FRQ": "Degree of Frequency",
    "ITN": "Degree of Iteration",
    "SPT": "Specified Points in Calendrical Time",
    "ITE": "Iterations Per Time-Period",
    "ILT": "Iterations Per Long-Term",
    "TPP": "Temporal Position Relative to Present",
    "ANT": "Degree of Anticipation",
    "RTI": "Relative Timeline Indicator",
    "AMD": "Ante-Meridiem Times of Day",
    "PMD": "Post-Meridiem Times of Day",
    "TME": "Degree of Timeliness",
    "TPR": "Temporal Placement/Reaction/Viewpoint",
    "STG": "Stage, Step or Phase of a Process",
    "STS": "Spatio-Temporal Specification",
    "DTS": "Duration of Shift/Change/Transition",
    "VTS": "Variability of Shift/Change/Transition",

    # Sec 4.3: Spatial, Positional and Motion Affixes
    "IPE": "Internal Positional Orientation of a (Quasi-)Long Object Having an Orientational 'End' or 'Ends'",
    "IPF": "Internal Positional Orientation of an Object Having an Orientational 'Face' or 'Front'",
    "PFL": "Internal Positional Orientation of a (Quasi-)Flat Object",
    "SVS": "Spatial/Areal/Volumetric Scale",
    "SWM": "Simultaneous Withdrawal/Pulling Motion",
    "RCO": "Recoil in Place",
    "TFI": "Back & Forth; To & Fro in Place",
    "CRV": "Cycle or Circular Motion over Linear Vector",
    "MDL": "Random Pattern of Modulation",
    "ICR": "Increase",
    "ICD": "Increase then Decrease",
    "IDR": "Random/Varying Increases and Decreases",
    "FLS": "Degree of Fluctuation/Stability",
    "MVT": "Degree of Stillness/Movement",

    # Sec 4.4: Qualifying Affixes
    "MDN": "Degree of Age or Modernity",
    "STY": "Stylistic thru Time",
    "NTR": "Degree of Notoriety, Acceptance, Respect, Honor",
    "DSN": "Degree of Seniority",
    "QUA": "Degree of Quality, Effectiveness or Adequacy",
    "DOP": "Degree of Optimality",
    "UNQ": "Degree of Specialness or Uniqueness",
    "CNQ": "Degree of Consequentiality, Finality or Irrevocability",
    "APR": "Degree of Contextual Appropriateness",
    "HRC": "Position on Social, Occupational, or (Para-)Military Hierarchy",
    "BEH": "Degree of Subjective Effect or Behavioral Appropriateness",
    "SCA": "Degree of Social Appropriateness",
    "ITL": "Degree of Intelligence Manifested",
    "PRB": "Degree of Probability or Likelihood",
    "FRM": "Degree of Formality",
    "TYP": "Degree of Typicalness",
    "PTY": "Degree of Prototypicality",
    "ENT": "Degree of Enthusiasm",
    "LCK": "Degree of Luck/Fortune",
    "REA": "Degree of Genuineness or Veracity",
    "DRE": "Degree of Recreation",
    "DSA": "Degree of Societal Agreement/Consensus",
    "CNT": "Degree of Centrality",
    "DPY": "Degree of Principality",
    "DCP": "Degree of Concentration vs. Dissipation",
    "PRT": "Degree of Portability",
    "FGN": "Degree of Foreignness",
    "STR": "Degree of Physical Strength Possible or Used",
    "FRC": "Degree of Physical Force Exerted",
    "ITY": "Degree of Intensity",
    "RLV": "Degree of Relevancy",
    "NEC": "Degree of Necessity",

    # Sec 4.5: Adverbial/Relational Affixes
    "DCV": "Degree of Spatio-Temporal Divergence or Convergence",
    "DPT": "Depiction/Representation/Record",
    "MEA": "Means for Being or Doing",
    "SUF": "Degree of Sufficiency of Amount/Impact",
    "EXD": "Dynamically Changing Degree or Extent",
    "ESO": "Extent to Which Something Occurs",
    "TVP": "Trivalent Polarity",
    "PLT": "Pletive",
    "INL": "Degree of Inclusion",
    "PTT": "Non-Contiguous (i.e., Intermixed) Portion of the Whole",
    "PTW": "Contiguous Portion of the Whole",
    "DGR": "Exactness of Degree",

    # Sec 4.6: Agential/Participative Affixes
    "AGN": "Degree of Agency, Intent or Effectiveness",
    "ATN": "Degree of Attention or Determination",
    "CAP": "Degree of Potential or Capability",
    "DPD": "Degree of Dependency",
    "CRL": "Degree of Physical Control",
    "LET": "to let X be/happen/manifest...",
    "EFT": "Degree of Effort",
    "MAT": "Degree of Maturation",
    "MNN": "Manner",
    "PLN": "Degree of Planning",
    "PCN": "Degree of Care, Precision or Scrutiny",
    "ERR": "Degree of Correctness versus Error",
    "SBT": "Degree of Subtlety/Nuance",
    "CVT": "Degree of Covertness versus Overtness",
    "DCY": "Degree of Certainty",
    "DSR": "Degree of Discretion or Secrecy",
    "PLE": "Degree of Emotional/Mental (Dis-)Pleasure",
    "EFP": "Degree of Effect from Pain or Pleasure",
    "OPF": "Degree of Operational Functionality",
    "PRV": "Degree of Prevention or Blockage",
    "DNG": "Degree of Danger",
    "MEC": "Type of Mechanical Instrumentality",
    "ENB": "Means of Enablement",
    "TRS": "Degree of Trustworthiness/Reliability",
    "XPT": "Expectation of Outcome",
    "MOT": "Degree of Self-Conscious Deliberation or Motivation",
    "CHC": "Degree of Choice by Externally-Induced Agent",
    "IMP": "Degree of Impact on Patient/Target or Enablement of Outcome",
    "DLB": "Degree of Deliberateness/Agency",
    "CNS": "Degree of Consent",
    "DSD": "Desiderative",
    "OBG": "Obligative/Necessitative",
    "MTA": "Negative Motivation for an Action or State",
    "AVS": "Aversive",
    "FLX": "Degree of Rigidity/Flexibility",
    "DCX": "Degree of Complexity",
    "SPH": "Degree of Sophistication",
    "MAK": "Making/Constructing",
    "AUT": "Authorization",
    "SOC": "Type of Social Instrumentality",
    "EML": "Emulation",
    "CTC": "Degree of Contact",
    "FRO": "Functional Role",
    "SRO": "Subjective Role",
    "ARO": "Adverse Role",
    "PRQ": "Prerequisite",

    # Sec 4.7: Modifying Affixes
    "DSG": "Degree of Design or Organization",
    "PPF": "Degree of Practice-Based Proficiency",
    "KPF": "Degree of Knowledge-Based Proficiency",
    "KSD": "Generational/Sequential Kinship Descriptions",
    "ASC": "Areal/Spatial Scope",
    "DDM": "Degree of Domestication",
    "CVL": "Degree of Civilization",

    # Positional affixes
    "HND": "Hand Position",
    "FP1": "Finger Positions 1",
    "FP2": "Finger Positions 2",
    "BOD": "Position of One's Core Body",
    "AR1": "Position of One's Arms 1",
    "AR2": "Position of One's Arms 2",
    "BY1": "Bodily Instrumentality 1",
    "BY2": "Bodily Instrumentality 2",
    "BY3": "Bodily Instrumentality 3",
    "BY4": "Bodily Instrumentality 4",

    # Sec 4.8: Sensory Affixes
    "VSR": "Visible Spectral Regions",
    "NVS": "Non-Visible Spectral Regions",
    "CLD": "Color Dimensions",
    "COL": "Color Attributes",
    "OLF": "Olfactory Associations",
    "TCT": "Tactile Associations",
    "VIS": "Visual Associations",
    "AUR": "Aural Associations",
    "GST": "Gustatory Associations",
    "MST": "Degree of Moisture",
    "SNX": "Sensory Experience or Reaction",
    "EMQ": "Emotional Qualification",

    # Mathematical/Measurement Affixes
    "BAO": "Basic Arithmetical Operations",
    "SMO": "Secondary Mathematical Operations",
    "TGO": "Trigonometric Operations",
    "NDB": "Non-Decimal Number Bases",
    "VAM": "Velocity and Acceleration Measurement",
    "LDA": "Linear Dimensional Measurement A",
    "LDB": "Linear Dimensional Measurement B",
    "ARM": "Areal Measurement",
    "VMA": "Volumetric Measurement A",
    "VMB": "Volumetric Measurement B",
    "VMC": "Volumetric Measurement C",
    "USM": "Ultra-Short Temporal Measurement",
    "TPM": "Temporal Measurement",
    "LTM": "Long-Term Temporal Measurement",
    "EMU": "Energy Measurement Units",
    "FMU": "Force Measurement Units",
    "PMU": "Pressure Measurement Units",
    "OEM": "Other Energy Measurement Units",
    "MAS": "Measurement of Mass",
    "TMS": "Temperature Measurement System",
    "CUA": "Currency A",
    "HG1": "Linguistic Hedges (1st Group)",
    "HG2": "Linguistic Hedges (2nd Group)",

    # Configurative/Meta-level Affixes
    "EPS": "Energy/Power Source",
    "LCM": "Life-Cycle of a Mechanism/Device/Fixture/Machine",
    "MNS": "Maintenance of a State",
    "PWF": "Part/Whole Functional Metaphors",
    "PWC": "Part/Whole Componential Metaphors",
    "DCF": "2nd-Level Duplex Configurative Set of 1st-Level Sets",
    "MCF": "2nd-Level Multiplex Configurative Set of 1st-Level Sets",
    "DPR": "Dispersion or Separability of a Configurative Set",
    "SDP": "Subset of Duplex Set",
    "CGL": "Motive/Reason for Conglomeration of Gestalt Entity",
    "ITG": "Integrity of a Configurative Set",
    "SYS": "Networks & Systems",
    "SMN": "Semantic Network",
    "ERN": "Entity and Relationship Within a Semantic Network",
    "PXM": "Degree of Proximity",
    "EPP": "Edible or Autonomous Plant Parts/Components",
    "MMA": "Metaphor / Metonym / Allusion",
    "MET": "Metonymic Categories",
    "CNM": "Type of Container/Packaging",
    "MCD": "Manufactured/Specialized Containers/Dispensers",
    "CVY": "Means of Transportation/Conveyance",
    "BMP": "Building, Structure, or Meeting Place",
    "KBP": "Basis for System of Knowledge, Belief or Practice",
    "PSA": "Personal Association",
    "MMV": "Medium Via/Through/Along Which",
    "ISA": "Infrastructural Association",
    "FNS": "Non-Solid Material States",
    "FMS": "Material States/Forms (Solids)",
    "AMS": "Additional Material States/Forms (Malleable Solid)",
    "ACS": "Access Point",
    "TRF": "Transformation for Use",
    "DFB": "Derived Foodstuff/Beverage",
    "MLT": "Mealtimes",
    "CK1": "Cooking Methods 1",
    "CK2": "Cooking Methods 2",
    "CK3": "Cooking Methods 3",
    "PLC": "Stage of Annual Plant Life-Cycle",
    "FSP": "Degree of Freshness or Spoilage",
    "TEM": "Temperature",
    "UDE": "Undesirable Entailments",

    # Coordinative and Connective Affixes
    "PXR": "Partially Exclusive OR",
    "XOR": "Exclusive Or and Contrastive Adverbials",
    "ADT": "Adverbial Additives",
    "DST": "Distributive Coordination",
    "CVY": "Means of Transportation/Conveyance",
    "CPP": "Capacity As Patient",
    "SIA": "Status of Inferred Arguments",
    "VRF": "Verifiability of Info & Trustworthiness of its Source",
    "APP": "Appraisal of Outcome/Effect",
    "LVL": "Alternate Forms of Comparison Operators (Levels)",
    "COS": "Comparison Specifications",
    "SCL": "Standards for Comparison for Use with Levels",
    "VWP": "From Viewpoint/Perspective of",
    "ROC": "Result / Outcome / Consequences",
    "HDM": "Hidden or Double Meaning",
    "URS": "Usage of a Resource",

    # Aspect Affixes
    "AP1": "Aspects I",
    "AP2": "Aspects II",
    "AP3": "Aspects III",
    "AP4": "Aspects IV",

    # Grammatical affixes
    "AVP": "Misc. Adverbial Phrases",
    "VAL": "Valence",
    "IVL": "Illocution + Validation",
    "PTN": "Potential",

    # Biological/Species Affixes
    "PG1": "Physical Features Based on Geography 1",
    "PG2": "Physical Features Based on Geography 2",
    "BRE": "Biogeographic Realm/Ecozone",
    "GEO": "Geographic or Environmental Niche",
    "WBN": "Water-based Environmental Niche",
    "OBN": "Ocean-based Environmental Niche",
    "ENS": "Environmental Niche",
    "DBM": "Distinctive Bodily/Fur Markings",
    "MB1": "Morphological Bodily Distinctions 1",
    "MB2": "Morphological Bodily Distinctions 2",
    "PEB": "Overall Presence/Participation Within an Environmental Niche or Biosphere",
    "PZE": "Paleozoic Era",
    "MZE": "Mesozoic Era",
    "ACQ": "Acquisition",
    "RNC": "Renunciation",
    "ENG": "Degree of Engagement/Involvement",
    "OSQ": "Opportunity Squandered",
    "MEM": "Memory",
    "NNN": "N-Numbered Angles/Facets/Faces/Sides/Points/Forms",
    "PSS": "Degree of Alienable Possession",
    "CLS": "Classification",
    "BAS": "Basis",
    "VPF": "Variance From Prototypical 3-D Form",
    "TDP": "Three-Dimensional Polyhedral Forms",
    "CNW": "Conway Polyhedral Mathematical Operations",
    "HUM": "Human Usage/Presence/Intervention",
    "APV": "Alternate Points of View",

    # Identity/Cultural/Geographic Affixes
    "SID": "Subjective Identity",
    "EGY": "Initiating Force/Energy of a Motion/Trajectory/Appearance/Change-in-State",
    "MSE": "Miscellaneous Entailments Associated with an Entity",
    "STF": "Miscellaneous Surface Textures or Formations",
    "MIA": "Musical Instrument Attributes",
    "CNC": "Degree of Concern or Consideration",
    "ASP": "Attributes of Sub-Atomic Particles",
    "ION": "Ionic Compounding Elements",
    "AMC": "Number of Atoms in a Molecular Compound",
    "CPI": "Polyatomic Cations",

    # Areal/Cultural Association Affixes
    "AAA": "Areal/Cultural Association - East Asia",
    "AAS": "Areal/Cultural Association - South Asia",
    "AAI": "Areal/Cultural Association - Southeast Asia / East Indies",
    "AAC": "Areal/Cultural Association - Central Asia",
    "AAH": "Areal/Cultural Association - Northern Africa & Horn of Africa",
    "AAF": "Areal/Cultural Association - Sub-Saharan Africa",
    "AAN": "Areal/Cultural Association - Northern & Western Europe",
    "AAE": "Areal/Cultural Association - Southern & Eastern Europe",
    "AAW": "Areal/Cultural Association - Western Hemisphere",
    "AAO": "Areal/Cultural Association - Other Areas",

    # Health/Bodily affixes
    "HEA": "State of Health",
    "ADI": "Adverse Disease/Disorder/Illness",
    "CRD": "Degree of Crowdedness",

    # Motion-related modifying affixes
    "DLB": "Degree of Deliberateness/Agency",

    # Additional from pages 81-88
    "IPE": "Internal Positional Orientation of a (Quasi-)Long Object Having an Orientational 'End' or 'Ends'",
}

# ============================================================================
# MANUAL_TYPES: Override type for specific entries where phonological rules
# don't match (exceptions, edge cases, or from PDF direct reading)
# ============================================================================
MANUAL_TYPES = {
    # From existing JSON patterns and previous PDF reading
    "DX1": "0",
    "DX2": "0",
    "IDF": "0",
    "EXT": "0*",
    "FAM": "0",
    "SIM": "0*",
    "SEQ": "0*",
    "SQT": "0",
    "TNX": "0*",
    "ELA": "0",
    "SQC": "0",
    "ASQ": "0",
    "TPI": "0",
    "TD1": "0*",
    "TD2": "0*",
    "LTD": "0*",
    "LGD": "0*",
    "EPC": "0*",
    "UST": "0",
    "STT": "0",
    "ST2": "0",
    "FRQ": "0*",
    "ITN": "0",
    "TPP": "0*",
    "ANT": "0*",
    "RTI": "0",
    "AMD": "0",
    "PMD": "0",
    "TME": "0*",
    "TPR": "0",
    "STG": "0*",
    "STS": "0*",
    "DTS": "0",
    "VTS": "0",
    "QUA": "0*",
    "DOP": "0",
    "UNQ": "0*",
    "CNQ": "0*",
    "APR": "0",
    "HRC": "0",
    "BEH": "0",
    "ITL": "0",
    "PRB": "0",
    "FRM": "0",
    "TYP": "0*",
    "PTY": "0",
    "ENT": "0",
    "STR": "0*",
    "FRC": "0*",
    "ITY": "0*",
    "RLV": "0",
    "REA": "0*",
    "DRE": "0",
    "DSA": "0",
    "CNT": "0",
    "DPY": "0",
    "DCP": "0*",
    "PRT": "0",
    "FGN": "0",
    "SVS": "0",
    "TVP": "D1",
    "PLT": "0",
    "RCO": "0",
    "TFI": "0",
    "CRV": "0",
    "MDL": "0",
    "ICR": "0",
    "ICD": "0",
    "IDR": "0",
    "FLS": "0*",
    "MVT": "0*",
    "MDN": "0",
    "STY": "0",
    "NTR": "0",
    "DSN": "0",
    "SWM": "0",
    "IPE": "0",
    "IPF": "0",
    "PFL": "0",
    "SUF": "0*",
    "EXD": "0",
    "INL": "0",
    "PTT": "0",
    "PTW": "0*",
    "DGR": "0",
    "ESO": "0",
    "SCA": "0",
    "LCK": "0*",
    "NEC": "0",
    "ERN": "0",
    "MEM": "0",
    "ENG": "0",
    "OSQ": "0",
    "PSS": "0",
    "CLS": "0",
    "BAS": "0",
    "NNN": "0",
}


def main():
    with open('data/affixes.json') as f:
        affixes = json.load(f)

    desc_updated = 0
    type_updated = 0
    desc_missing = 0

    for affix in affixes:
        abbrev = affix['abbrev']

        # Update description if missing
        if not affix.get('description'):
            if abbrev in DESCRIPTIONS:
                affix['description'] = DESCRIPTIONS[abbrev]
                desc_updated += 1
            else:
                desc_missing += 1
                print(f"  WARNING: No description for {abbrev} (cs={affix['cs']})", file=sys.stderr)

        # Update type if missing
        if not affix.get('type'):
            if abbrev in MANUAL_TYPES:
                affix['type'] = MANUAL_TYPES[abbrev]
            else:
                affix['type'] = derive_type(affix['cs'])
            type_updated += 1

    with open('data/affixes.json', 'w') as f:
        json.dump(affixes, f, indent=2, ensure_ascii=False)
        f.write('\n')

    print(f"Updated {desc_updated} descriptions, {type_updated} types")
    if desc_missing:
        print(f"Still missing {desc_missing} descriptions")


if __name__ == '__main__':
    main()
