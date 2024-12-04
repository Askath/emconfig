#!/bin/bash

# Directory containing the folders
TARGET_DIR="."

# Loop through each directory in the target directory
for dir in "$TARGET_DIR"/*; do
    # Check if it's a directory
    if [ -d "$dir" ]; then
        # Extract the base directory name
        base_name=$(basename "$dir")
        # Remove any trailing version numbers and keep only the package name
        new_name=$(echo "$base_name" | sed -E 's/-[0-9]+(\.[0-9]+)*(\.[0-9a-zA-Z]+)*$//')
        # Rename the directory if the new name is different
        if [ "$base_name" != "$new_name" ]; then
            mv "$dir" "$TARGET_DIR/$new_name"
            echo "Renamed: $base_name -> $new_name"
        fi
    fi
done

echo "Renaming completed."
