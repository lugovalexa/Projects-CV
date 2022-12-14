import glob
import pandas as pd
import json
from datetime import datetime
from tqdm import tqdm

output_folder = "output/"
actual_time = datetime.now()

global_frame = []

def extractor(file, works, position, hashtag):
    print(f"working on file {position+1} // {works}")
    with open(file, encoding="utf-8") as jsonfile:
        data = json.load(jsonfile)

        if data["meta"]["result_count"] == 0:
            print("No results")
            return "empty"
        else:
            tweets = data["data"]

            general_df = []

            for element in tweets:
                try:
                    # Get the author name
                    list_of_users = data["includes"]["users"]
                    author_id = element["author_id"]
                    for user in list_of_users:
                        if user["id"] == author_id:
                            username = user["username"]
                        else:
                            pass

                    # Get Media Attributes
                    media_duration = 0
                    media_views = 0

                    try:
                        media_lists = element["attachments"]["media_keys"]
                        media_meta = data["includes"]["media"]

                        # Multiple media elements in a single tweet need to place each element in a list and output them in the same cell.
                        media_type_list = []
                        media_url_list = []


                        for media in media_lists:
                            for media_attached in media_meta:
                                if media == media_attached["media_key"]:
                                    media_type = media_attached["type"]
                                    media_type_list.append(media_type)
                                    if media_type == "photo":
                                        media_url = media_attached["url"]
                                        media_url_list.append(media_url)
                                    else:
                                        media_url = "false"
                                        media_url_list.append(media_url)

                                    if media_type == "video":
                                        ms_media_duration = media_attached["duration_ms"]
                                        media_duration = ms_media_duration / 1000
                                        media_views = media_attached["public_metrics"]["view_count"]
                                    else:
                                        pass
                                else:
                                    pass
                    except KeyError:
                        media_type_list = ["false"]
                        media_url_list = ["false"]

                    # In Reply To ID
                    reply_to_name = "false"
                    try:
                        reply_to_id = element["in_reply_to_user_id"]
                        entities = element["entities"]
                        mentions = entities["mentions"]
                        for mention in mentions:
                            if reply_to_id == mention["id"]:
                                reply_to_name = mention["username"]
                            else:
                                pass
                    except KeyError:
                        reply_to_id = "false"
                        reply_to_name = "false"

                    # Get entities

                    # Hashtag list
                    try:
                        entities = element["entities"]
                        hastags = entities["hashtags"]
                        hashtag_list = []
                        for hastag in hastags:
                            hash = hastag["tag"]
                            hashtag_list.append(hash)
                            hashtags_string = ";".join(hashtag_list)
                    except KeyError:
                        hashtags_string = "false"

                    # MENTIONS


                    try:
                        entities = element["entities"]
                        mentions = entities["mentions"]
                        mentions_list = []
                        for mention in mentions:
                            user = mention["username"]
                            mentions_list.append(user)
                            mentions_string = ";".join(mentions_list)
                    except KeyError:
                        mentions_string = "false"

                    # Anotations TYPE
                    try:
                        entities = element["entities"]
                        anotations = entities["annotations"]
                        anotations_type_list = []
                        anotations_element_list = []
                        for anotation in anotations:
                            type = anotation["type"]
                            anotations_type_list.append(type)
                            text = anotation["normalized_text"]
                            anotations_element_list.append(text)
                            annotations_type_string = ";".join(anotations_type_list)
                            annotations_elements_string = ";".join(anotations_element_list)

                    except KeyError:
                        annotations_type_string = "false"
                        annotations_elements_string = "false"

                    # GEO DATA

                    try:
                        geo = element["geo"]
                        try:
                            place_id = geo["place_id"]
                            places = data["includes"]["places"]
                            for place in places:
                                if place["id"] == place_id:
                                    place_name = place["full_name"]
                                else:
                                    pass
                        except KeyError:
                            place_id = "false"
                            place_name = "false"
                            pass
                        try:
                            coordinates = geo["coordinates"]["coordinates"]
                            list_lat_lon = []
                            for coordinate in coordinates:
                                coord_string = str(coordinate)
                                list_lat_lon.append(coord_string)
                            coordinates_string = ";".join(list_lat_lon)
                        except KeyError:
                            coordinates_string = "false"
                            pass
                    except KeyError:
                        place_id = "false"
                        coordinates_string = "false"
                        place_name = "false"

                    # Get Year
                    year = element["created_at"].split("-")[0]

                    # Generate the Dataframe
                    df = pd.DataFrame({
                        "retrieved_at": actual_time,
                        "tweet_id": element["id"],
                        "tweet_created_at": element["created_at"],
                        "tweet_year": year,
                        "query": hashtag,
                        "sensitive": element["possibly_sensitive"],
                        "lang": element["lang"],
                        "source": element["source"],
                        "username": username,
                        "user_id": element["author_id"],
                        "text": element["text"],
                        "in_reply_to_id": reply_to_id,
                        "in_reply_to_name": reply_to_name,
                        "rt_count": element["public_metrics"]["retweet_count"],
                        "reply_count": element["public_metrics"]["reply_count"],
                        "like_count": element["public_metrics"]["like_count"],
                        "quote_count": element["public_metrics"]["quote_count"],
                        "has_media": [media_type_list],
                        "if_video_duration": media_duration,
                        "if_video_views": media_views,
                        "media_url": [media_url_list],
                        "tweet_url": f"https://twitter.com/{username}/status/{element['id']}",
                        "ent_hashtags": hashtags_string,
                        "ent_mentions": mentions_string,
                        "ent_anotation_types": annotations_type_string,
                        "ent_anotation_elements": annotations_elements_string,
                        "place_id": place_id,
                        "place_name": place_name,
                        "coordinates": coordinates_string,
                    }, index=[0])

                    general_df.append(df)
                except(KeyError, IndexError):
                    pass

            final_df = pd.concat(general_df)

            return final_df

def crontroller(filename, hashtag, capture_name):
    files = glob.glob(f"{capture_name}/api_responses/{filename}*.json")
    print(files)
    works = len(files)
    for file in tqdm(files, desc="Pharsing JSON files..."):
        position = files.index(file)
        dataframe = extractor(file, works, position, hashtag)
        global_frame.append(dataframe)
    print("Creating df...")
    try:
        export_frame = pd.concat(global_frame)
        print("exporting df")
        export_frame.to_csv(f"{capture_name}/dataset-{filename}.csv", index=False, sep=",",quotechar='"', line_terminator="\n")
        print("Done!")
        global_frame.clear()
    except (ValueError, TypeError):
        print("Nothing to Export")
        pass
