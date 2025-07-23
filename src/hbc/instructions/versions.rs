use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// HBC version information
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HbcVersion {
    /// HBC version number
    pub version: u32,
    /// Git tag for this version
    pub tag: Option<String>,
    /// Optional commit hash
    pub commit: Option<String>,
    /// Optional release date
    pub release_date: Option<String>,
}

impl HbcVersion {
    /// Create a new HBC version
    pub fn new(
        version: u32,
        tag: Option<String>,
        commit: Option<String>,
        release_date: Option<String>,
    ) -> Self {
        Self {
            version,
            tag,
            commit,
            release_date,
        }
    }

    /// Get the cache directory path for this version
    pub fn cache_dir(&self) -> PathBuf {
        PathBuf::from("cache").join(format!("v{}", self.version))
    }

    /// Get the path to BytecodeList.def file
    pub fn def_file_path(&self) -> PathBuf {
        self.cache_dir()
            .join("include/hermes/BCGen/HBC/BytecodeList.def")
    }

    /// Get the path to BytecodeVersion.h file
    pub fn header_file_path(&self) -> PathBuf {
        self.cache_dir()
            .join("include/hermes/BCGen/HBC/BytecodeVersion.h")
    }
}

/// Registry of all supported HBC versions
#[derive(Debug, Clone)]
pub struct VersionRegistry {
    versions: HashMap<u32, HbcVersion>,
    min_version: u32,
    max_version: u32,
}

impl VersionRegistry {
    /// Create a new version registry
    pub fn new() -> Self {
        Self {
            versions: HashMap::new(),
            min_version: u32::MAX,
            max_version: 0,
        }
    }

    /// Add a version to the registry
    pub fn add_version(&mut self, version: HbcVersion) {
        let version_num = version.version;
        self.min_version = self.min_version.min(version_num);
        self.max_version = self.max_version.max(version_num);
        self.versions.insert(version_num, version);
    }

    /// Get a version by number
    pub fn get_version(&self, version: u32) -> Option<&HbcVersion> {
        self.versions.get(&version)
    }

    /// Get all versions
    pub fn versions(&self) -> impl Iterator<Item = &HbcVersion> {
        self.versions.values()
    }

    /// Get minimum supported version
    pub fn min_version(&self) -> u32 {
        self.min_version
    }

    /// Get maximum supported version
    pub fn max_version(&self) -> u32 {
        self.max_version
    }

    /// Check if a version is supported
    pub fn is_supported(&self, version: u32) -> bool {
        self.versions.contains_key(&version)
    }

    /// Load registry from JSON file
    pub fn load_from_file(path: &PathBuf) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let data: RegistryData = serde_json::from_str(&content)?;

        let mut registry = Self::new();
        for version_data in data.versions {
            registry.add_version(version_data);
        }

        Ok(registry)
    }

    /// Save registry to JSON file
    pub fn save_to_file(&self, path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
        let data = RegistryData {
            min_version: self.min_version,
            max_version: self.max_version,
            versions: self.versions.values().cloned().collect(),
        };

        let content = serde_json::to_string_pretty(&data)?;
        std::fs::write(path, content)?;
        Ok(())
    }
}

/// Data structure for JSON serialization
#[derive(Debug, Serialize, Deserialize)]
struct RegistryData {
    min_version: u32,
    max_version: u32,
    versions: Vec<HbcVersion>,
}

#[cfg(test)]
mod tests {
    use crate::generated::VERSION_REGISTRY;

    #[test]
    fn test_version_registry() {
        let registry = &*VERSION_REGISTRY;
        // Pick a known version from versions.json
        assert!(registry.is_supported(80));
        assert!(registry.is_supported(96));
        assert!(!registry.is_supported(50));
    }
}
