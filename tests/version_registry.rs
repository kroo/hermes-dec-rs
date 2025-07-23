use hermes_dec_rs::hbc::instructions::versions::{HbcVersion, VersionRegistry};

#[test]
fn test_version_registry_manual() {
    // Create a version registry manually
    let mut registry = VersionRegistry::new();
    
    // Add some test versions
    registry.add_version(HbcVersion::new(80, Some("v0.8.0".to_string()), None, Some("2021-04-29".to_string())));
    registry.add_version(HbcVersion::new(95, None, Some("f6b56d3".to_string()), Some("2023-03-29".to_string())));
    registry.add_version(HbcVersion::new(96, None, Some("e0fc671".to_string()), Some("2023-08-16".to_string())));
    
    // Test that all versions are present
    assert!(registry.is_supported(80));
    assert!(registry.is_supported(95));
    assert!(registry.is_supported(96));
    
    // Test that unsupported versions are not present
    assert!(!registry.is_supported(50));
    assert!(!registry.is_supported(97));
    
    // Test min/max version functions
    assert_eq!(registry.min_version(), 80);
    assert_eq!(registry.max_version(), 96);
    
    // Test that we can get version information
    let version_80 = registry.get_version(80);
    assert!(version_80.is_some());
    let version_80 = version_80.unwrap();
    assert_eq!(version_80.version, 80);
    assert_eq!(version_80.tag, Some("v0.8.0".to_string()));
    assert_eq!(version_80.release_date, Some("2021-04-29".to_string()));
    
    // Test version 96
    let version_96 = registry.get_version(96);
    assert!(version_96.is_some());
    let version_96 = version_96.unwrap();
    assert_eq!(version_96.version, 96);
    assert_eq!(version_96.commit, Some("e0fc671".to_string()));
    assert_eq!(version_96.release_date, Some("2023-08-16".to_string()));
    
    // Test that all versions have the correct data
    for version in registry.versions() {
        assert_eq!(version.version, version.version); // Sanity check
        println!("Version {}: tag={:?}, commit={:?}, date={:?}", 
                 version.version, version.tag, version.commit, version.release_date);
    }
}

#[test]
fn test_version_registry_from_json() {
    // Test loading from the JSON file
    let versions_file = std::path::PathBuf::from("src/hbc/instructions/versions.json");
    let versions_content = std::fs::read_to_string(versions_file).expect("Failed to read versions.json");
    
    #[derive(serde::Deserialize)]
    struct VersionsFile {
        versions: Vec<HbcVersion>,
    }
    
    let versions_file: VersionsFile = serde_json::from_str(&versions_content).expect("Failed to parse versions.json");
    let versions = &versions_file.versions;
    
    // Create registry from JSON data
    let mut registry = VersionRegistry::new();
    for version in versions {
        registry.add_version(version.clone());
    }
    
    // Test that all versions from JSON are present
    assert!(registry.is_supported(51));
    assert!(registry.is_supported(80));
    assert!(registry.is_supported(95));
    assert!(registry.is_supported(96));
    
    // Test min/max version functions
    assert_eq!(registry.min_version(), 51);
    assert_eq!(registry.max_version(), 96);
    
    // Test that we can get version information
    let version_80 = registry.get_version(80);
    assert!(version_80.is_some());
    let version_80 = version_80.unwrap();
    assert_eq!(version_80.version, 80);
    assert_eq!(version_80.commit, Some("e70045d".to_string()));
    assert_eq!(version_80.release_date, Some("2021-01-13".to_string()));
    
    // Test version 96
    let version_96 = registry.get_version(96);
    assert!(version_96.is_some());
    let version_96 = version_96.unwrap();
    assert_eq!(version_96.version, 96);
    assert_eq!(version_96.commit, Some("e0fc671".to_string()));
    assert_eq!(version_96.release_date, Some("2023-08-16".to_string()));
    
    println!("Successfully loaded {} versions from JSON", versions.len());
    for version in registry.versions() {
        println!("Version {}: tag={:?}, commit={:?}, date={:?}", 
                 version.version, version.tag, version.commit, version.release_date);
    }
} 