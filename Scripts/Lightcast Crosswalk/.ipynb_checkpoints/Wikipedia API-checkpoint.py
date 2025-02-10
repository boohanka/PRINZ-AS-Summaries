import requests
import re
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("wiki_digger")

def get_district_from_wikipedia(market_name):
    def fetch_page_content(name):
        formatted_name = name.replace(" ", "_")
        url = f"https://en.wikipedia.org/w/api.php?action=query&prop=revisions&titles={formatted_name}&rvprop=content&rvslots=main&format=json&redirects=1"
        logger.info(f"Fetching URL: {url}")
        try:
            response = requests.get(url)
            response.raise_for_status()  # Raises an error for non-200 status
        except requests.RequestException as e:
            logger.error(f"Error fetching page for {name}: {e}")
            return None, None
        
        try:
            data = response.json()
        except ValueError as e:
            logger.error(f"Error parsing JSON for {name}: {e}")
            return None, None

        pages = data.get("query", {}).get("pages", {})
        page_id = next(iter(pages), None)
        
        if page_id == "-1":
            logger.warning(f"Page does not exist for {name}")
            return None, None

        raw_content = pages[page_id].get("revisions", [{}])[0].get("slots", {}).get("main", {}).get("*", "")
        return raw_content, name

    def extract_district(raw_content):
        # Updated regex to capture district information and avoid Wikipedia templates
        district_pattern = re.compile(
            r"\|\s*(district|shire_district|county|administrative_district|administrative_area|local_authority|unitary_authority|unitary_england)\s*=\s*([^\|\{\}\n]+)",  # Stop capture at |, {, }, or newline
            re.IGNORECASE
        )
        match = district_pattern.search(raw_content)
        if match:
            district = match.group(2).strip()
            # Additional cleanup to remove any lingering markup or special characters
            district = re.sub(r"\[\[|\]\]|\{\{.*?\}\}", "", district).split("|")[0].strip()
            logger.info(f"Match found: {district}")
            return district
        logger.info("No district information found.")
        return None

    # Define name variants
    original_name = market_name.split(",")[0].strip()  # Part before the comma
    first_word = original_name.split()[0]  # First word if original_name has two words
    
    # Determine variants based on structure of the first part
    name_variants = [market_name]  # Full name
    
    # Add the primary name without the county part (i.e., part before the comma)
    name_variants.append(original_name)
    
    # If the first part before the comma contains exactly two words, try only the first word
    if len(original_name.split()) == 2:
        name_variants.append(first_word)

    # Add a variant that removes hyphenated parts, if any (e.g., "Town-on-Sea" -> "Town")
    name_variants.append(original_name.split("-")[0].strip())

    # Filter out duplicate entries in case some variants are the same
    name_variants = list(dict.fromkeys(name_variants))
    
    # Try each name variant in sequence until district is found
    district = None
    previous_name = None  # Track the previous name to avoid duplicates
    for i, name in enumerate(name_variants):
        # Skip this variant if it's the same as the previous one
        if name == previous_name:
            logger.info(f"Skipping duplicate name variant: '{name}'")
            continue
        
        logger.info(f"Attempting with name variant {i + 1}: '{name}'")
        raw_content, title = fetch_page_content(name)
        
        if raw_content:
            logger.info(f"Content found using name variant: '{name}'")
            district = extract_district(raw_content)
            
            # Stop further attempts if district information is found
            if district:
                logger.info(f"District information found using name variant: '{name}'")
                break  # Exit the loop as soon as district is found

        # Update the previous name to the current one for the next iteration
        previous_name = name

    if not district:
        logger.warning(f"No district information found for {market_name}")
    return district



import pandas as pd

#df = pd.read_csv("Data/Temp/new/**INSERT REMAINING MARKETS**.csv")
df['lad'] = df['market_name'].apply(get_district_from_wikipedia)
df.to_csv("data/aux_data/new/Markets Lads API.csv", index = False)