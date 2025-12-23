# =========================================================
# config_endpoints.R
# Centralized configuration for ADMET endpoint metadata
# Aligned with Global_ADMET_Index_Math.txt (v1.2)
# Supports: ADMETlab 3.0, pkCSM, SwissADME, DeepPK
# =========================================================

#' Define the master endpoint metadata
get_endpoint_meta <- function() {
  tibble::tribble(
    ~pattern, ~canonical, ~group, ~direction, ~L, ~U, ~is_prob_bad, ~weight_in_group,

    # =========================================================
    # 1. ABSORPTION (Weight: 0.20)
    # =========================================================
    
    # --- HIA ---
    "^hia$|^intestinal.absorption..human..$|^gi.absorption$|^.absorption/human.intestinal.absorption..probability$", 
    "HIA", "Absorption", "higher_better", 0, 1, FALSE, 0.40,

    # --- Caco-2 ---
    "^caco2$|^caco2.permeability$|^.absorption/caco-2..logpaap...predictions$",
    "Caco2", "Absorption", "higher_better", -6, 1, FALSE, 0.30,

    # --- P-gp ---
    "^pgp_sub$|^p-glycoprotein.substrate$|^pgp.substrate$|^.absorption/p-glycoprotein.substrate..probability$",
    "Pgp_substrate", "Absorption", "prob_bad", 0, 1, TRUE, 0.10,
    
    "^pgp_inh$|^p-glycoprotein.i.inhibitor$|^p-glycoprotein.ii.inhibitor$|^.absorption/p-glycoprotein.inhibitor..probability$",
    "Pgp_inhibitor", "Absorption", "prob_bad", 0, 1, TRUE, 0.10,

    # --- Bioavailability ---
    "^f20$|^f30$|^f50$|^bioavailability.score$|^.absorption/human.oral.bioavailability.20...probability$|^.absorption/human.oral.bioavailability.50...probability$",
    "Bioavailability", "Absorption", "higher_better", 0, 1, FALSE, 0.10,

    # --- Lipophilicity (logP) ---
    "^logp$|^xlogp3$|^wlogp$|^mlogp$|^consensus.log.p$|^.general.properties/log.p...predictions$",
    "logP", "Absorption", "mid_ideal", 0, 5, FALSE, 0.10,

    # =========================================================
    # 2. DISTRIBUTION (Weight: 0.15)
    # =========================================================

    # --- BBB ---
    "^bbb$|^bbb.permeability$|^cns.permeability$|^bbb.permeant$|^.distribution/blood-brain.barrier..probability$",
    "BBB", "Distribution", "prob_penetration", -2, 1, FALSE, 0.40,

    # --- PPB ---
    "^ppb$|^.distribution/plasma.protein.binding..predictions$",
    "PPB", "Distribution", "lower_better", 0, 100, TRUE, 0.30,

    # --- Fraction Unbound (Fu) ---
    "^fu$|^fraction.unbound..human..$|^.distribution/fraction.unbound..human...predictions$",
    "Fu", "Distribution", "higher_better", 0, 1, FALSE, 0.30,

    # --- VDss ---
    "^logvdss$|^vdss..human..$|^.distribution/steady.state.volume.of.distribution..predictions$",
    "VDss", "Distribution", "mid_ideal", -1, 1, FALSE, 0.20,

    # =========================================================
    # 3. METABOLISM (Weight: 0.20)
    # =========================================================
    
    # --- CYP Inhibitors ---
    "^cyp1a2.*inh.*|^cyp1a2.inhibitor$|^.metabolism/cyp.1a2.inhibitor..probability$", "CYP1A2_Inh", "Metabolism", "prob_bad", 0, 1, TRUE, 0.2,
    "^cyp2c9.*inh.*|^cyp2c9.inhibitior$|^.metabolism/cyp.2c9.inhibitor..probability$", "CYP2C9_Inh", "Metabolism", "prob_bad", 0, 1, TRUE, 0.2,
    "^cyp2c19.*inh.*|^cyp2c19.inhibitior$|^.metabolism/cyp.2c19.inhibitor..probability$", "CYP2C19_Inh", "Metabolism", "prob_bad", 0, 1, TRUE, 0.2,
    "^cyp2d6.*inh.*|^cyp2d6.inhibitior$|^cyp2d6.inhibitor$|^.metabolism/cyp.2d6.inhibitor..probability$", "CYP2D6_Inh", "Metabolism", "prob_bad", 0, 1, TRUE, 0.2,
    "^cyp3a4.*inh.*|^cyp3a4.inhibitior$|^cyp3a4.inhibitor$|^.metabolism/cyp.3a4.inhibitor..probability$", "CYP3A4_Inh", "Metabolism", "prob_bad", 0, 1, TRUE, 0.2,

    # --- CYP Substrates ---
    "^cyp.*sub.*|^cyp.*substrate$|^.metabolism/cyp.*.substrate..probability$", 
    "CYP_Substrate", "Metabolism", "prob_bad", 0, 1, TRUE, 0.1,

    # =========================================================
    # 4. EXCRETION (Weight: 0.10)
    # =========================================================

    # --- Clearance (CL) ---
    "^cl-plasma$|^total.clearance$|^.excretion/clearance..predictions$",
    "Clearance", "Excretion", "lower_better", 0, 50, TRUE, 0.60,

    # --- Half-Life (t1/2) ---
    "^t0.5$|^t1/2$|^.excretion/half-life.of.drug..predictions$",
    "Half_Life", "Excretion", "higher_better", 0.5, 24, FALSE, 0.40,

    # =========================================================
    # 5. TOXICITY (Weight: 0.25)
    # =========================================================

    # --- hERG ---
    "^herg$|^herg-10um$|^herg.i.inhibitor$|^herg.ii.inhibitor$|^.toxicity/herg.blockers..probability$",
    "hERG", "Toxicity", "prob_bad", 0, 1, TRUE, 1.0,

    # --- Ames ---
    "^ames$|^ames.toxicity$|^.toxicity/ames.mutagenesis..probability$",
    "Ames", "Toxicity", "prob_bad", 0, 1, TRUE, 1.0,

    # --- DILI ---
    "^dili$|^hepatotoxicity$|^.toxicity/liver.injury.i...dili...probability$",
    "DILI", "Toxicity", "prob_bad", 0, 1, TRUE, 1.0,

    # --- Carcinogenicity ---
    "^carcinogenicity$|^.toxicity/carcinogenesis..probability$",
    "Carcinogenicity", "Toxicity", "prob_bad", 0, 1, TRUE, 1.0,

    # --- LD50 ---
    "^ld50_oral$|^oral.rat.acute.toxicity..ld50..$|^.toxicity/rat..acute...predictions$",
    "LD50", "Toxicity", "higher_better", 1, 5000, FALSE, 0.5,

    # --- Skin Sensitization ---
    "^skinsen$|^skin.sensitisation$|^.toxicity/skin.sensitisation..probability$",
    "SkinSensitization", "Toxicity", "prob_bad", 0, 1, TRUE, 0.8,

    # --- Environmental ---
    "^minnow.toxicity$|^.toxicity/fathead.minnow..predictions$",
    "AquaticTox", "Toxicity", "higher_better", -5, 5, FALSE, 0.5,

    # =========================================================
    # 6. PHYSICOCHEMICAL (Weight: 0.10)
    # =========================================================

    "^mw$|^mol_weight$|^molecular.weight$", "MW", "Physicochemical", "mid_ideal", 100, 600, FALSE, 0.20,
    "^tpsa$|^topological.polar.surface.area$", "TPSA", "Physicochemical", "mid_ideal", 20, 140, FALSE, 0.20,
    "^nrot$|^#rotatable_bonds$|^#rotatable.bonds$", "RotatableBonds", "Physicochemical", "lower_better", 0, 10, TRUE, 0.20,
    "^nhd$|^#donors$|^#h-bond.donors$", "HBD", "Physicochemical", "lower_better", 0, 5, TRUE, 0.10,
    "^nha$|^#acceptors$|^#h-bond.acceptors$", "HBA", "Physicochemical", "lower_better", 0, 10, TRUE, 0.10,
    "^qed$", "QED", "Physicochemical", "higher_better", 0, 1, FALSE, 0.30,
    "^fsp3$|^fraction.csp3$", "Fsp3", "Physicochemical", "higher_better", 0.4, 1, FALSE, 0.10,
    "^logs$|^esol.log.s$|^.general.properties/log.s..predictions$", "Solubility", "Physicochemical", "higher_better", -6, 0, FALSE, 0.20 
  )
}

#' Map a property string to its canonical metadata
map_property <- function(prop) {
  endpoint_meta <- get_endpoint_meta()
  prop_l <- tolower(prop)
  
  for (i in seq_len(nrow(endpoint_meta))) {
    if (stringr::str_detect(prop_l, stringr::regex(endpoint_meta$pattern[i], ignore_case = TRUE))) {
      return(endpoint_meta[i, ])
    }
  }
  return(tibble::tibble(canonical = prop, group = "Other", direction = "unknown", L = NA_real_, U = NA_real_, is_prob_bad = NA, weight_in_group = 1))
}

#' Get default group weights
get_group_weights <- function() {
  c(Absorption = 0.20, Distribution = 0.15, Metabolism = 0.20, Excretion = 0.10, Toxicity = 0.25, Physicochemical = 0.10)
}