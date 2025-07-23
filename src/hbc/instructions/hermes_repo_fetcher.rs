use super::versions::{HbcVersion, VersionRegistry};
use std::path::PathBuf;
use std::process::Command;
use std::collections::HashMap;
use std::fs;
use anyhow::{Result, Context};

/// Source files for a specific HBC version
#[derive(Debug, Clone)]
pub struct SourceFiles {
    /// Path to BytecodeList.def file
    pub def_file: PathBuf,
    /// Path to BytecodeVersion.h file
    pub header_file: PathBuf,
    /// Version information
    pub version: HbcVersion,
}

/// Source fetcher for Hermes bytecode definitions
pub struct SourceFetcher {
    /// Cache directory for downloaded files
    cache_dir: PathBuf,
    /// Hermes repository URL
    hermes_repo_url: String,
    /// Local git repository path
    repo_path: PathBuf,
}

impl SourceFetcher {
    /// Create a new source fetcher
    pub fn new(cache_dir: PathBuf) -> Result<Self> {
        let hermes_repo_url = "https://github.com/facebook/hermes.git".to_string();
        let repo_path = cache_dir.join("hermes-repo");
        
        // Ensure cache directory exists
        fs::create_dir_all(&cache_dir)
            .context("Failed to create cache directory")?;
        

        
        Ok(Self {
            cache_dir,
            hermes_repo_url,
            repo_path,
        })
    }
    
    /// Initialize the Hermes repository
    pub fn init_repository(&self) -> Result<()> {
        if !self.repo_path.exists() {
            let status = Command::new("git")
                .args(&["clone", "--tags", &self.hermes_repo_url, &self.repo_path.to_string_lossy()])
                .status()
                .context("Failed to clone Hermes repository")?;
            
            if !status.success() {
                anyhow::bail!("Git clone failed with status: {}", status);
            }
        } else {
            // Always fetch tags to ensure we have the latest
            let status = Command::new("git")
                .current_dir(&self.repo_path)
                .args(&["fetch", "--tags"])
                .status()
                .context("Failed to fetch Hermes tags")?;
            
            if !status.success() {
                anyhow::bail!("Git fetch failed with status: {}", status);
            }
        }
        
        Ok(())
    }
    
    /// Fetch source files for a specific version
    pub fn fetch_version(&self, version: &HbcVersion) -> Result<PathBuf> {
        let version_dir = self.cache_dir.join(format!("v{}", version.version));
        let def_file_path = version_dir.join("BytecodeList.def");
        
        // Check if we already have this version cached
        if def_file_path.exists() {
            return Ok(def_file_path);
        }
        
        // Create version directory
        fs::create_dir_all(&version_dir)
            .context("Failed to create version directory")?;
        
        // Initialize repository if needed
        self.init_repository()?;
        
        // Clean up any stale git lock files
        let lock_file = self.repo_path.join(".git/index.lock");
        if lock_file.exists() {
            fs::remove_file(&lock_file)
                .context("Failed to remove stale git lock file")?;
        }
        
        // Checkout the specific version
        let checkout_ref = if let Some(commit) = &version.commit {
            commit
        } else {
            version.tag.as_ref().unwrap()
        };
        
        let status = Command::new("git")
            .current_dir(&self.repo_path)
            .args(&["checkout", checkout_ref])
            .status()
            .context("Failed to checkout version")?;
        
        if !status.success() {
            anyhow::bail!("Git checkout failed with status: {}", status);
        }
        
        // Copy BytecodeList.def to cache
        let source_def_path = self.repo_path.join("include/hermes/BCGen/HBC/BytecodeList.def");
        
        if !source_def_path.exists() {
            // Try alternative paths
            let alt_paths = [
                "lib/VM/BytecodeList.def",
                "include/hermes/VM/BytecodeList.def",
                "hermes/lib/VM/BytecodeList.def",
                "VM/BytecodeList.def",
                "hermes/include/hermes/BCGen/HBC/BytecodeList.def",
            ];
            
            let mut found = false;
            for alt_path in &alt_paths {
                let alt_source = self.repo_path.join(alt_path);
                if alt_source.exists() {
                    fs::copy(&alt_source, &def_file_path)
                        .context("Failed to copy BytecodeList.def")?;
                    found = true;
                    break;
                }
            }
            
            if !found {
                anyhow::bail!("BytecodeList.def not found in any expected location for version {}", version.version);
            }
        } else {
            fs::copy(&source_def_path, &def_file_path)
                .context("Failed to copy BytecodeList.def")?;
        }
        Ok(def_file_path)
    }
    
    /// Fetch source files for all versions in the registry
    pub fn fetch_all_versions(&self, registry: &VersionRegistry) -> Result<HashMap<u32, PathBuf>> {
        let mut fetched_files = HashMap::new();
        
        for version in registry.versions() {
            match self.fetch_version(version) {
                Ok(path) => {
                    fetched_files.insert(version.version, path);
                }
                Err(e) => {
                    println!("cargo:warning=Failed to fetch version {}: {}", version.version, e);
                    // Continue with other versions
                }
            }
        }
        
        Ok(fetched_files)
    }
    
    /// Clean up temporary files
    pub fn cleanup(&self) -> Result<()> {
        // Keep the cache for future builds, but clean up any temporary files
        Ok(())
    }
}

impl Clone for SourceFetcher {
    fn clone(&self) -> Self {
        Self {
            cache_dir: self.cache_dir.clone(),
            hermes_repo_url: self.hermes_repo_url.clone(),
            repo_path: self.repo_path.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    
    #[test]
    fn test_source_fetcher_creation() {
        let temp_dir = TempDir::new().unwrap();
        let fetcher = SourceFetcher::new(temp_dir.path().to_path_buf());
        assert!(fetcher.is_ok());
    }
    
    #[tokio::test]
    async fn test_version_registry_integration() {
        // Use a persistent cache directory to avoid re-cloning the Hermes repo on every test run
        let cache_dir = std::path::PathBuf::from("target/hermes-repo-cache");
        let fetcher = SourceFetcher::new(cache_dir).unwrap();
        
        // Create a test version registry
        let mut registry = VersionRegistry::new();
        registry.add_version(HbcVersion::new(80, Some("v0.8.0".to_string()), None, None ));
        
        // This will fail in CI without git, but that's expected
        let result = fetcher.fetch_all_versions(&registry);
        // We don't assert on the result since it depends on external git access
        println!("Fetch result: {:?}", result);
    }
} 